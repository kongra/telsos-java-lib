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

(defn str-trim-left (s) (ch-string)
  (ch-string s)
  (string-left-trim *whitespaces* s))

(defn str-trim-right (s) (ch-string)
  (ch-string s)
  (string-right-trim *whitespaces* s))

(defn str-trim (s) (ch-string)
  (ch-string s)
  (string-trim *whitespaces* s))

(defn str-collapse-whitespaces (s) (ch-string)
  (ch-string s)
  (ppcre:regex-replace-all "\\s+" s " "))

(defn str-replace-first (old new s) (ch-string)
  (ch-string old)
  (ch-string new)
  (ch-string   s)
  (let* ((ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old)))
    (ppcre:regex-replace old s (list new))))

(defn str-replace-all (old new s) (ch-string)
  (ch-string old)
  (ch-string new)
  (ch-string   s)
  (let* ((ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old)))
    (ppcre:regex-replace-all old s (list new))))

(defn str-join (sep &rest strings) (ch-string)
  (ch-string sep)
  (let ((sep (str-replace-all "~" "~~" (string sep))))
    (format nil
            (concatenate 'string "~{~a~^" sep "~}")
            strings)))

(defn str-concat (&rest strings) (ch-string)
  (apply #'concatenate 'string strings))

(defn str-repeat (n s) (ch-string)
  (ch-fixnum-nat n)
  (ch-string     s)
  (let ((result nil))
    (dotimes (i n)
      (setf result (cons s result)))
    (apply #'str-concat result)))
