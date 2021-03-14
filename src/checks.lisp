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
            (assert (,',pred ,x) (,x))
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

;; TODO: fixnum-nat-p, (integer-)nat-p
;; TODO: fixnum-pos-p, (integer-)pos-p

;; CHARACTERS
(def-check-type-ch ch-base-char         base-char)
(def-check-type-ch ch-standard-char standard-char)
(def-check-type-ch ch-extended-char extended-char)
(def-check-type-ch ch-character         character)

;; STRINGS
(def-check-type-ch ch-simple-string simple-string)
(def-check-type-ch ch-string               string)

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
(def-check-type-ch ch-simple-vector simple-vector)
(def-check-type-ch ch-vector               vector)
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
       (assert ,x (,x))
       ,x)))

;; NILABLE VALUES
(defmacro ch-maybe ((check)  &body body)
  (with-gensyms (x)
    `(let ((,x (progn ,@body)))
       (if (not ,x)
           ,x
           (,check ,x)))))
