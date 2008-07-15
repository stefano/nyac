;  Copyright (c) 2008 Dissegna Stefano
;  Released under the terms of the GNU LGPL

(set separators* '(#\Space #\Newline #\Tab))
(set special-chars* '(#\; #\( #\) #\[ #\] #\' #\, #\`))
(set eof* "end-of-file")

(def skip-line (stream)
  (let c (readc stream)
    (if (not (is c #\Newline))
      (skip-line stream))))

(def read-table (list))

(def read/tbl (stream read-table)
  (withs (c (readc stream)
          f (assoc-f read-table c stream))
    (if 
      (nullp c) eof*
      f (do
          (ungetc stream c)
          (f stream read-table))
      (mem c separators*) (read/tbl stream read-table)
      (is c #\;) (do
                   (skip-line stream)
                   (read/tbl stream read-table))
      (err (str-append "Unknown char: " (string c))))))

(def read-symbol (s tbl)
  (with (l nil
         c nil)
    (fwhile (fn () (set c (readc s)) (and (not (mem c separators*))
                                          (not (mem c special-chars*))))
      (fn ()
        (set l (cons c l))))
    (ungetc s c)
    (with (str (mkstr (len l))
           l (rev l)
           i 0)
      (feach (fn (c) (do (str-set str i c) (set i (+ i 1)))) l)
      (if (iso str "t") t 
          (iso str "nil") nil
          (intern str)))))

; read-symbol must be the last, because it catches (almost) everything
(set read-table 
  (cons 
    (cons 
      (fn (x y) 
        (and (not (mem x special-chars*)) (not (mem x separators*))))
      read-symbol)
    read-table))

(def adigit (c)
  (and (>= (char->fx c) (char->fx #\0)) (<= (char->fx c) (char->fx #\9))))

(def read-num (s tbl)
  (withs (c (readc s)
          sign (if (is c #\-) -1 1)
          n (if (is c #\-) 0 (- (char->fx c) (char->fx #\0))))
    (set c (readc s))
    (fwhile (fn () (adigit c))
      (fn ()
        (set n (+ (* n 10) (- (char->fx c) (char->fx #\0))))
        (set c (readc s))))
    (if (is c #\.)
      (let p 10.0
        (set c (readc s))
        (fwhile (fn () (adigit c))
          (fn ()
            (set n (+ n (/ (- (char->fx c) (char->fx #\0)) p)))
            (set p (* p 10))
            (set c (readc s))))))
    (ungetc s c)
    (* sign n)))

(def read-num-p (c stream)
  (if (and c (adigit c))
    t
    (if (is c #\-)
       (let next (readc stream)
         (ungetc stream next)
         (if (adigit next)
           t)))))

(set read-table (cons (cons read-num-p read-num) read-table))

(def read-string (s tbl)
  (with (escape-next nil
         res nil
         c #\Null)
    (readc s) ; skip first #\"
    (fwhile (fn () 
              (set c (readc s))
              (not (and (is c #\") (not escape-next))))
      (fn ()
        (if 
          escape-next (do 
                        (set escape-next nil) 
                        (set res (cons (escape-char c) res)))
          (is c #\\) (set escape-next t)
          (not (is c #\")) (set res (cons c res)))))
    (with (str (mkstr (len res))
           res (rev res)
           i 0)
      (feach (fn (c) (do (str-set str i c) (set i (+ i 1)))) res)
      str)))

(let escape-table '((#\" . #\")
                    (#\\ . #\\)
                    (#\n . #\Newline)
                    (#\t . #\Tab))
  (def escape-char (c)
    (let res (cdr (assoc c escape-table))
      (if res res (err (make-string "Don't know how to escape char " c))))))

(set read-table (cons (cons #\" read-string) read-table))

(def read-quote (s tbl)
  (readc s) ; skip '
  (list 'quote (read/tbl s tbl)))

(def read-quasiquote (s tbl)
  (readc s) ; skip `
  (list 'quasiquote (read/tbl s tbl)))

(def read-unquote (s tbl)
  (readc s) ; skip ,
  (let next (readc s)
    (if (is next #\@) ; splice
      (list 'splice (read/tbl s tbl))
      (do
        (ungetc s next)
        (list 'unquote (read/tbl s tbl))))))

(set read-table (append (list (cons #\' read-quote)
                              (cons #\` read-quasiquote)
                              (cons #\, read-unquote))
                        read-table))

(def read-char-p (c stream)
  (and 
    (is c #\#)
    (let next (readc stream)
      (ungetc stream next)
      (is next #\\))))

(let name->char '((Newline . #\Newline)
                  (Tab . #\Tab)
                  (Null . #\Null)
                  (Space . #\Space)
                  (\ . #\\))
  (def read-char (s tbl)
    (readc s) ; skip #
    (readc s) ; skip \
    (let c (readc s)
      (if (or (mem c separators*) (mem c special-chars*))
        c
        (do
          (ungetc s c)
          (let word (read-symbol s tbl)
            (if (is (str-len (to-string word)) 1)
              (str-ref (to-string word) 0)
              (let res (assoc word name->char)
                (if (not res)
                  (err (make-string "Unknown char name: " word))
                  (cdr res))))))))))
;    (let c (readc s)
;      (if (is c #\\)
;        (let next (readc s)
;          (if (or (mem next separators*) (mem next special-chars*))
;            (do (ungetc s next) #\\)
;            (escape-char next)))
;        c)))

(set read-table (cons (cons read-char-p read-char) read-table))

(def skip-separators (s)
  (let c (readc s)
    (if (mem c separators*)
      (skip-separators s)
      (ungetc s c))))

(def read-list-with-ter (s tbl ter)
  (skip-separators s)
  (let next (readc s)
    (if (is next ter) 
      nil
      (do 
        (ungetc s next)
        (with (the-car (read/tbl s tbl)
               the-cdr nil
               c nil)
          (skip-separators s)
          (set c (readc s))
          (if (is c #\.)
            (do
              (set the-cdr (read/tbl s tbl))
              (skip-separators s)
              (if (not (is (readc s) ter))
                (err "More than one object follows ."))
              (cons the-car the-cdr))
            (is c ter) (cons the-car nil)
            (do (ungetc s c)
                (cons the-car (read-list-with-ter s tbl ter)))))))))

(def read-list (s tbl)
  (readc s) ; skip (
  (read-list-with-ter s tbl #\)))

(set read-table (cons (cons #\( read-list) read-table))

(def read-vector-p (c stream)
  (and 
    (is c #\#)
    (let next (readc stream)
      (ungetc stream next)
      (is next #\())))

(def read-vector (s tbl)
  (readc s) ; skip #
  (apply vector (read-list s tbl)))

(set read-table (cons (cons read-vector-p read-vector) read-table))

(def read-square-bracket (s tbl)
  (readc s) ; skip [
  (let body (read-list-with-ter s tbl #\])
    (list 'fn (list '_) body)))

(set read-table (cons (cons #\[ read-square-bracket) read-table))

(def readall (in-stream)
  (read-list-with-ter in-stream read-table nil))
