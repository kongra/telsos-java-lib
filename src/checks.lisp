;;;; Created 2021-03-14

(in-package #:telsos)

;; ABSTRACTION / MACROS
(defmacro def-check-type-ch (name typespec)
  (let ((x 'x))
    `(defmacro ,name (&body body)
       (with-gensyms (,x)
         `(let ((,x (progn ,@body)))
            (check-type ,x ,',typespec)
            ,x)))))

(defmacro def-assert-ch (name pred)
  (let ((x 'x))
    `(defmacro ,name (&body body)
       (with-gensyms (,x)
         `(let ((,x (progn ,@body)))
            (assert (,',pred ,x))
            ,x)))))

;; NUMBERS
(def-check-type-ch ch-fixnum             fixnum)
(def-check-type-ch ch-bignum             bignum)
(def-check-type-ch ch-integer           integer)
(def-check-type-ch ch-short-float   short-float)
(def-check-type-ch ch-double-float double-float)
(def-check-type-ch ch-float               float)
(def-check-type-ch ch-ratio               ratio)
(def-check-type-ch ch-rational         rational)
(def-check-type-ch ch-real                 real)
(def-check-type-ch ch-complex           complex)
(def-check-type-ch ch-number             number)

;; NUMBER PREDICATES
(def-assert-ch ch-zerop   zerop)
(def-assert-ch ch-plusp   plusp)
(def-assert-ch ch-minusp minusp)
(def-assert-ch ch-evenp   evenp)
(def-assert-ch ch-oddp     oddp)

(def-assert-ch ch-fixnum-nat  non-negative-fixnum-p)
(def-assert-ch ch-fixnum-pos      positive-fixnum-p)
(def-assert-ch ch-integer-nat non-negative-integer-p)
(def-assert-ch ch-integer-pos     positive-integer-p)

(declaim (inline fixnum-in-p))
(defun fixnum-in-p (n start end)
  (declare (fixnum n start end))
  (<= start n end))

(defmacro ch-fixnum-in ((start end) &body body)
  (with-gensyms (n)
    `(let ((,n (progn ,@body)))
       (assert (fixnum-in-p ,n ,start ,end))
       (the fixnum ,n))))

;; CHARACTERS
(def-check-type-ch ch-base-char         base-char)
(def-check-type-ch ch-standard-char standard-char)
(def-check-type-ch ch-extended-char extended-char)
(def-check-type-ch ch-character         character)

;; STRINGS
(def-check-type-ch ch-simple-string simple-string)
(def-check-type-ch ch-string               string)

(declaim (inline whitespace-p))
(defun whitespace-p (c)
  (declare (character c))
  (or (eq c #\Space)
      (eq c #\Newline)
      (eq c #\Backspace)
      (eq c #\Tab)
      (eq c #\Linefeed)
      (eq c #\Page)
      (eq c #\Return)
      (eq c #\Rubout)))

(defun blank-p (s)
  (declare (type (simple-array character) s))
  (let ((n (the fixnum (length s))))
   (dotimes (i n)
     (declare (fixnum i))
     (unless (whitespace-p (char s i))
       (return-from blank-p nil))))
  t)

(declaim (inline non-blank-p))
(defun non-blank-p (s)
  (not (blank-p s)))

(def-assert-ch ch-blank         blank-p)
(def-assert-ch ch-non-blank non-blank-p)

;; SYMBOLS
(def-check-type-ch ch-symbol symbol)

;; KEYWORDS
(def-check-type-ch ch-keyword keyword)

;; BOOLEANS
(def-check-type-ch ch-boolean boolean)

;; LISTS
(def-check-type-ch ch-cons cons)
(def-check-type-ch ch-list list)

;; ARRAYS
(def-check-type-ch ch-simple-array simple-array)
(def-check-type-ch ch-array               array)

;; VECTORS
(def-check-type-ch ch-simple-vector         simple-vector)
(def-check-type-ch ch-vector                       vector)
(def-check-type-ch ch-simple-bit-vector simple-bit-vector)
(def-check-type-ch ch-bit-vector               bit-vector)

;; SEQUENCES
(def-check-type-ch ch-sequence sequence)

;; HASH-TABLES
(def-check-type-ch ch-hash-table hash-table)

;; NON-NIL (SOME)
(defmacro ch-some (&body body)
  (with-gensyms (x)
    `(let ((,x (progn ,@body)))
       (assert ,x)
       ,x)))

;; NILABLE VALUES
(defmacro ch-maybe ((check) &body body)
  (with-gensyms (x)
    `(let ((,x (progn ,@body)))
       (if (not ,x)
           ,x
           (,check ,x)))))

;; CHECKS IN PROCEDURES
(defmacro defn (spec args check &body body)
  (let* ((check (if (symbolp check) (list check) check))
         (body  (concatenate 'list check body)))

    `(defun ,spec ,args ,body)))

(defmacro fn (args check &body body)
  (let* ((check (if (symbolp check) (list check) check))
         (body  (concatenate 'list check body)))

    `(lambda ,args ,body)))
