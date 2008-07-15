;;;;  Copyright (c) 2008 Dissegna Stefano
;;;;  Released under the terms of the GNU LGPL

(def print-str (str)
  (ffi-call "a_write" 1 str (str-len str))
  nil)

(def newline ()
  (print-str "
"))

(def c-count (i)
  (if (fx= i 0)
      0
      (fx+ 1 (c-count (fx/ i 10)))))

(def c-fill (str i index)
  (if (fx= i 0)
      str
      (do
       (str-set str index (fx->char (fx+ (char->fx #\0) (fxrem i 10))))
       (c-fill str (fx/ i 10) (fxsub1 index)))))

(def fx->str (i)
  (if (fx= i 0)
      "0"
      (withs (nc (c-count i) 
              str (mkstr nc))
        (c-fill str i (fxsub1 (str-len str))))))

(def char->str (c)
  (string #\# #\\ c))

(def list->str-a (l)
  (if 
    (and l (consp l)) (make-string " " (car l) (list->str-a (cdr l)))
    l (make-string " . " (to-string l) ")")
    ")"))

(def list->str (l)
  (make-string "(" (list->str-a l)))
;  (str-append
;    (reduce str-append (map (fn (x) (str-append (to-string x) " ")) l) "(") 
;    ")"))

(def str-copy (from to start i)
  (if (fx< i (str-len from))
      (do
       (str-set to (fx+ start i) (str-ref from i))
       (str-copy from to start (fxadd1 i)))
      to))

(def str-append (s1 s2)
  (let str (mkstr (fx+ (str-len s1) (str-len s2)))
    (do
     (str-copy s1 str 0 0)
     (str-copy s2 str (str-len s1) 0)
      str)))

(def trunc (x)
  (let res (round x)
    (if (fl> (fx->fl res) x) (fxsub1 res) res)))

(def decimal-part (x) (- x (trunc x)))

(def fill-float-str (x str i)
  (if (fx= i (str-len str))
    str
    (let x (* x 10.0)
      (str-set str i (fx->char (fx+ (trunc x) (char->fx #\0))))
      (fill-float-str (decimal-part x) str (fxadd1 i)))))

(def fl->str (x n-digits)
  (let str (mkstr n-digits)
    (make-string (trunc x) "." (fill-float-str (decimal-part x) str 0))))

(def to-string (x)
  (if (fxp x) (if (>= x 0) (fx->str x) (make-string "-" (fx->str (- x))))
      (flp x) (if (fl>= x 0.0) (fl->str x 16) 
                               (make-string "-" (fl->str (- x) 16)))
      (charp x) (char->str x)
      (symbolp x) (get-symbol-string x)
      (strp x) x
      (consp x) (list->str x)
      (nullp x) "nil"
      (is x t) "t"
      (fnp x) "#<function>"
      "#<unknown object>"))

(def print (x)
  (print-str (to-string x))
  (newline))

(def test ()
  (print-str (fx->str 18097))
  (newline)
  (print-str (char->str #\A))
  (newline)
  (print-str (str-append "aa" "bb"))
  (newline))

;(test)
