(in-package #:plan-convert)

;;; Options -------------------------------------------------------------------

(defvar *template-path* NIL "Path of the Djula template to use to convert the .plan file")
(defvar *version* NIL "Application version")

(opts:define-opts
  (:name :help
   :description "print the help text and exit"
   :short #\h
   :long "help")
  (:name :template-path
   :description "use TEMPLATE-PATH as Djula template to convert the .plan file"
   :required T
   :long "template-path"
   :arg-parser #'identity
   :meta-var "TEMPLATE-PATH")
  (:name :version
   :description "print the version and exit"
   :short #\v
   :long "version"))

(define-condition exit (error)
  ((code
     :initarg :code
     :initform 0
     :reader exit-code))
  (:report (lambda (condition stream)
             (format stream "Trying to exit with code: ~S"
                     (exit-code condition)))))

(defun parse-opts (argv)
  (multiple-value-bind (options)
      (handler-case
          (handler-bind ((opts:missing-required-option (lambda (condition)
                                                         (if (or (member "-h" argv :test #'equal)
                                                                 (member "--help" argv :test #'equal)
                                                                 (member "-v" argv :test #'equal)
                                                                 (member "--version" argv :test #'equal))
                                                           (invoke-restart 'opts:skip-option)
                                                           (progn
                                                             (format t "~a~%" condition)
                                                             (error 'exit :code 1))))))
            (opts:get-opts argv))
        (opts:unknown-option (condition)
          (format t "~a~%" condition)
          (error 'exit :code 1))
        (opts:missing-arg (condition)
          (format t "~a~%" condition)
          (error 'exit :code 1)))
    (if (getf options :help)
      (progn
        (opts:describe
          :prefix "Reads a .plan file from stdin, and transform it using Djula (Django) templates"
          :args "[keywords]")
        (error 'exit)))
    (if (getf options :version)
      (progn
        (format T "~a~%" *version*)
        (error 'exit)))
    ; required arguments
    (setf *template-path* (getf options :template-path))))

;;; Utils ---------------------------------------------------------------------

(defvar *day-header-scanner* (ppcre:create-scanner "^# [0-9]{4}-[0-9]{2}-[0-9]{2}"))

(defun day-header-p (s)
  (ppcre:scan *day-header-scanner* s))

(defun eof-p (s)
  (eq s :eof))

(defun day-header-date (s)
  (second (split-sequence:split-sequence #\Space s)))

(defmacro hexadecimal-string (seq)
  `(format NIL "~{~(~2,'0x~)~}" (coerce ,seq 'list)))

;;; Stream --------------------------------------------------------------------

(defvar *last-line* NIL "Last, read, line")

(defun read-next ()
  (setf *last-line* (read-line NIL NIL :eof)))

(defun read-until (stop-p)
  (loop
    :for line = (read-next)
    :until (funcall stop-p line)
    :collect line))

(defun read-until-day-header ()
  (read-until (lambda (s) (or (eof-p s)
                              (day-header-p s)))))

(defun read-channel-description ()
  (format NIL "~{~A~^~%~}" (read-until-day-header)))

;;; Plan-day -----------------------------------------------------------------

(defstruct plan-day
  date content)

(defun plan-day-title (d)
  (day-header-date (plan-day-date d)))

(defun plan-day-sections (d)
  (let (sections section)
    (dolist (l (plan-day-lines d))
      (if (eql (getf l :type) :separator)
          (setf sections (cons (reverse section) sections)
                section nil)
          (push l section)))
    (when section
      (push (reverse section) sections))
    (nreverse sections)))

(defun plan-day-lines (d)
  (strip-trailing-newlines
    (collapse-collapsables
      (collapse-empties
        (parse-content-into-lines (plan-day-content d))))))

(defun parse-content-into-lines (s)
  (nreverse
    (reduce #'(lambda (acc s)
               (cons
                 (cond ((zerop (length s)) (list :type :empty))
                       ((string= s "---") (list :type :separator))
                       ((string= (subseq-safe s 0 2) "* ") (list :type :accomplished :content (subseq s 2)))
                       ((string= (subseq-safe s 0 2) "+ ") (list :type :fixed :content (subseq s 2)))
                       ((string= (subseq-safe s 0 2) "? ") (list :type :idea :content (subseq s 2)))
                       ((string= (subseq-safe s 0 2) "~ ") (list :type :discarded :content (subseq s 2)))
                       ((string= (subseq-safe s 0 2) "> ") (list :type :quoted :content (subseq s 2)))
                       ((string= (subseq-safe s 0 4) "    ") (list :type :snippet :content (subseq s 4)))
                       (t (list :type :generic :content s)))
                 acc))
            (split-sequence:split-sequence #\Newline s)
            :initial-value nil)))

(defun subseq-safe (sequence start &optional end)
  (when end
    (when (< (length sequence) end)
      (setf end nil)))
  (subseq sequence start end))

(defun collapse-empties (lines)
  (nreverse
    (reduce #'(lambda (acc l)
               (let ((cur-type (getf l :type))
                     (prev-type (getf (first acc) :type)))
                 (if (eql cur-type :empty)
                     (when (member prev-type '(:generic :snippet))
                       (let ((prev-content (getf (first acc) :content)))
                         (setf (getf (first acc) :content)
                               (concatenate 'string prev-content '(#\Newline)))))
                     (push l acc))
                 acc))
            lines
            :initial-value nil)))

(defun collapse-collapsables (lines)
  (nreverse
    (reduce #'(lambda (acc l)
               (let ((cur-type (getf l :type))
                     (prev-type (getf (first acc) :type)))
                 (if (and (eql cur-type prev-type) (member prev-type '(:generic :snippet)))
                   (let ((prev-content (getf (first acc) :content)))
                     (setf (getf (first acc) :content)
                           (concatenate 'string prev-content '(#\Newline) (getf l :content))))
                   (push l acc))
                 acc))
            lines
            :initial-value nil)))

(defun strip-trailing-newlines (lines)
  (prog1 lines
    (dolist (l lines)
      (setf (getf l :content) (string-strip-trailing-newlines (getf l :content))))))

(defun string-strip-trailing-newlines (string)
  (loop
    :with string = string
    :while (and string (string-ends-with-newline string))
    :do (setf string (subseq string 0 (1- (length string))))
    :finally (return string)))

(defun string-ends-with-newline (s)
  (char= (char s (1- (length s)))
         #\Newline))

(defun read-plan-day ()
  (unless (eof-p *last-line*)
    (make-plan-day :date *last-line* :content (read-channel-description))))

(defun read-most-recent-plan-days (max-items)
  (loop
    :with hq
    :for day = (read-plan-day)
    :while day
    :do (setf hq (merge 'list hq (list day) #'string< :key #'plan-day-title))
    :when (> (length hq) max-items) :do (pop hq)
    :finally (return (reverse hq))))

;;; Djula filters -------------------------------------------------------------

(djula:def-filter :rfc2822 (s)
  (let* ((parts (split-sequence:split-sequence #\- s))
         (parts (mapcar #'parse-integer parts))
         (d (dt:make-date (first parts) (second parts) (third parts))))
    (dt:rfc-2822 (dt:day+ d 1))))

(djula:def-filter :md5sum (s)
  (hexadecimal-string (md5:md5sum-string s)))

;;; Main ----------------------------------------------------------------------

(defun process-input ()
  (let ((description (read-channel-description))
        (days (loop :for day = (read-plan-day)
                    :while day
                    :collect day :into days
                    :finally (return (nreverse days)))))
    (djula:render-template* (pathname *template-path*) *standard-output*
                            :version *version*
                            :description (string-strip-trailing-newlines description)
                            :days (mapcar #'(lambda (day)
                                             (list
                                               :date (plan-day-title day)
                                               :content (string-strip-trailing-newlines (plan-day-content day))
                                               :sections (plan-day-sections day)))
                                          days)
                            :accomplished :accomplished
                            :fixed :fixed
                            :discarded :discarded
                            :idea :idea
                            :snippet :snippet
                            :quoted :quoted
                            :generic :generic)))

(defun toplevel()
  (handler-case (parse-opts (opts:argv))
    (exit (condition)
      (opts:exit (exit-code condition))))
  (process-input))
