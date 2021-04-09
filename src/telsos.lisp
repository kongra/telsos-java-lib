;;;; Created 2021-03-28

(in-package #:telsos)

(defmacro alambda (paramlist &body body)
  `(labels ((self ,paramlist ,@body))
     #'self))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
