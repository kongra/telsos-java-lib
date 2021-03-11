;;;; Created 2021-03-08

(asdf:defsystem #:telsos
  :description "The Elements of Style"
  :author      "Konrad Grzanek <kongra@gmail.com"
  :version     "0.1.0"
  :serial t
  :depends-on (#:alexandria
               #:iterate)

  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "telsos" :depends-on ("package"))))))
