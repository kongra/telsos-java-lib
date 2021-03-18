;;;; Created 2021-03-08

(in-package #:telsos)

;; STRING UTILS
(defvar *whitespaces*
  '(#\Space
    #\Newline
    #\Backspace
    #\Tab
    #\Linefeed
    #\Page
    #\Return
    #\Rubout))

(defun str-trim-left (s)
  (ch-string s)
  (string-left-trim *whitespaces* s))

(defun str-trim-right (s)
  (ch-string s)
  (string-right-trim *whitespaces* s))

(defun str-trim (s)
  (ch-string s)
  (string-trim *whitespaces* s))

(defun str-collapse-whitespaces (s)
  (ch-string s)
  (ppcre:regex-replace-all "\\s+" s " "))

(defun str-replace-first (old new s)
  (ch-string old)
  (ch-string new)
  (ch-string   s)
  (let* ((ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old)))
    (ppcre:regex-replace old s (list new))))

(defun str-replace-all (old new s)
  (ch-string old)
  (ch-string new)
  (ch-string   s)
  (let* ((ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old)))
    (ppcre:regex-replace-all old s (list new))))

(defun str-join (sep &rest strings)
  (ch-string sep)
  (let ((sep (str-replace-all "~" "~~" (string sep))))
    (format nil
            (concatenate 'string "~{~a~^" sep "~}")
            strings)))

(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

(defun str-repeat (n s)
  (ch-fixnum n)
  (ch-string s)
  (let ((result nil))
    (dotimes (i n)
      (setf result (cons s result)))
    (apply #'str-concat result)))
