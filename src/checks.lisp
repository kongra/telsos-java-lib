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

(defun fixnum-nat-p (n)
  (and (typep n 'fixnum)
       (>= (the fixnum n) 0)))

(defun integer-nat-p (n)
  (and (typep n 'integer)
       (>= (the integer n) 0)))

(defun fixnum-pos-p (n)
  (and (typep n 'fixnum)
       (> (the fixnum n) 0)))

(defun integer-pos-p (n)
  (and (typep n 'integer)
       (> (the integer n) 0)))

(def-assert-ch ch-fixnum-nat   fixnum-nat-p)
(def-assert-ch ch-fixnum-pos   fixnum-pos-p)
(def-assert-ch ch-integer-nat integer-nat-p)
(def-assert-ch ch-integer-pos integer-pos-p)

(defun fixnum-in-p (n start end)
  (check-type n     fixnum)
  (check-type start fixnum)
  (check-type end   fixnum)

  (<= (the fixnum start)
      (the fixnum     n)
      (the fixnum   end)))

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

(defun unsafe-whitespace-p (c)
  (declare (character c))
  (or (eq c #\Space)
      (eq c #\Newline)
      (eq c #\Backspace)
      (eq c #\Tab)
      (eq c #\Linefeed)
      (eq c #\Page)
      (eq c #\Return)
      (eq c #\Rubout)))

(defun unsafe-blank-p (s)
  (iter (for c :in-string s)
    (unless (unsafe-whitespace-p c)
      (return-from unsafe-blank-p nil)))
  t)

(defun unsafe-non-blank-p (s)
  (not (unsafe-blank-p s)))

(defun whitespace-p (c)
  (unsafe-whitespace-p (ch-character c)))

(defun blank-p (s)
  (unsafe-blank-p (ch-string s)))

(defun non-blank-p (s)
  (unsafe-non-blank-p (ch-string s)))

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
