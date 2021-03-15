;;;; Created 2021-03-08

(in-package #:telsos)

(defun blank-p (s)
  (iter (for c :in-string s)
    (unless (serapeum:whitespacep c)
      (return-from blank-p nil)))
  t)

(defun non-blank-p (s)
  (not (blank-p s)))

(def-assert-ch ch-blank         blank-p)
(def-assert-ch ch-non-blank non-blank-p)
