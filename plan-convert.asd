;;;; plan-convert.asd

(asdf:defsystem #:plan-convert
  :description "Matteo's .plan to _type_your_file_format_here_ converter"

  :author "Matteo Landi <matteo@matteolandi.net"
  :license  "MIT"

  :version "0.0.1"

  :depends-on (
    #:cl-ppcre
    #:md5
    #:simple-date-time
    #:split-sequence
    #:unix-opts #:djula
  )

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "plan-convert"
  :entry-point "plan-convert:toplevel"

  :serial t
  :components ((:file "package")
               (:module "src" :serial t
                        :components
                        ((:file "main")))))
