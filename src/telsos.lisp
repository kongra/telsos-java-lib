;;;; Created 2021-03-08

(in-package #:telsos)

(defun whitespace-p (c)
  (or (char= c #\Space)
      (char= c #\Tab)
      (char= c #\Linefeed)
      (char= c #\Return)
      (char= c #\Newline)
      (char= c #\Page)
      (char= c #\Vt)))
