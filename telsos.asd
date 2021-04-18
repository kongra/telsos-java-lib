;;;; Created 2021-03-08

(asdf:defsystem #:telsos
  :description "The Elements of Style"
  :author      "Konrad Grzanek <kongra@gmail.com"
  :version     "0.1.0"
  :serial t
  :depends-on (#:alexandria
               #:iterate
               #:cl-ppcre)

  :components ((:module "src"
                :components
                ((:file "packages")

                 (:file "checks"
                  :depends-on ("packages"))

                 (:file "str"
                  :depends-on ("packages"
                               "checks"))

                 (:file "telsos"
                  :depends-on ("packages"
                               "checks"
                               "str"))))))
