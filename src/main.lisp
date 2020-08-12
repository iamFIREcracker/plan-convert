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
                            :description description
                            :days (mapcar #'(lambda (day)
                                              (list
                                                :date (plan-day-title day)
                                                :content (plan-day-content day)))
                                          days))))

(defun toplevel()
  (handler-case (parse-opts (opts:argv))
    (exit (condition)
      (opts:exit (exit-code condition))))
  (process-input))

;;; REPL ----------------------------------------------------------------------

#+NIL
(setf *version* "0.0.1")

#+NIL
(setf *template-path* #P"template-html.htmldjango")

#+NIL
(setf *template-path* #P"template-rss.htmldjango")

#+NIL
(defun fake-input-stream ()
  (make-string-input-stream "This is my log ...

When I accomplish something, I write a * line that day.

Whenever a bug / missing feature / idea is mentioned during the day and I don't fix it, I make a note of it and mark it with ?.  Some things get noted many times before they get fixed.

Occasionally I go back through the old notes and mark with a + the things I have since fixed, and with a ~ the things I have since lost interest in.

--- Matteo Landi

# 2019-11-01
* xml-emitter: Add support for guid isPermaLink=false (https://github.com/VitoVan/xml-emitter/pull/3)
* xml-emitter: Add support for atom:link with rel=\"self\" (https://github.com/VitoVan/xml-emitter/pull/4)

# 2019-10-30
Finally A/I came back online, and I was finally able to create a request for a mailing list (to use it with the other college friends).  Anyway, the request has been created, so hopefully over the following days we will hear back from them...stay tuned!

"))

#+NIL
(let ((*standard-input* (fake-input-stream))) (process-input))
