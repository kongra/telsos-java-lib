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
  (string-left-trim *whitespaces* s))

(defun str-trim-right (s)
  (string-right-trim *whitespaces* s))

(defun str-trim (s)
  (string-trim *whitespaces* s))

(defun str-collapse-whitespaces (s)
  (ppcre:regex-replace-all "\\s+" s " "))

(defun str-replace-first (old new s)
  (let* ((ppcre:*allow-quoting* t)
         (old (concatenate 'string "\\Q" old)))
    (ppcre:regex-replace old s (list new))))

(defun str-replace-all (old new s)
  (let* ((ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old)))
    (ppcre:regex-replace-all old s (list new))))

(defun str-join (sep &rest strings)
  (let ((sep (str-replace-all "~" "~~" (string sep))))
    (format nil
            (concatenate 'string "~{~a~^" sep "~}")
            strings)))

(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

(defun str-repeat (n s)
  (declare (fixnum n) (optimize (speed 3)))
  (ch-fixnum-nat n)
  (let ((result nil))
    (dotimes (i n)
      (setf result (cons s result)))
    (apply #'str-concat result)))
