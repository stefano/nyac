;  Copyright (c) 2008 Dissegna Stefano
;  Released under the terms of the GNU LGPL

(def + (x y)
  (if (fxp x) (if (fxp y) (fx+ x y)
                  (flp y) (fl+ (fx->fl x) y)
                  (err "+: Wrong types"))
      (flp x) (if (flp y) (fl+ x y)
                  (fxp y) (fl+ x (fx->fl y))
                  (err "+: Wrong types"))
      (err "+: Wrong types")))

; Warning: the current implementation of '-' is _very_ slow, because
; it needs to cons its arguments
(def - args
  (let l (len args)
    (if (is l 0) 0
        (is l 1) (-/1 (car args))
        (is l 2) (-/2 (car args) (cadr args))
        (apply - (-/2 (car args) (cadr args)) (cddr args)))))

(def -/1 (x)
  (if (fxp x) (fx- 0 x)
      (flp x) (fl- 0.0 x)
      (err "-: Wrong types")))

(def -/2 (x y)
  (if (fxp x) (if (fxp y) (fx- x y)
                  (flp y) (fl- (fx->fl x) y)
                  (err "-: Wrong types"))
      (flp x) (if (flp y) (fl- x y)
                  (fxp y) (fl- x (fx->fl y))
                  (err "-: Wrong types"))
      (err "-: Wrong types")))

(def / (x y)
  (if (fxp x) (if (fxp y) (fx/ x y)
                  (flp y) (fl/ (fx->fl x) y)
                  (err "/: Wrong types"))
      (flp x) (if (flp y) (fl/ x y)
                  (fxp y) (fl/ x (fx->fl y))
                  (err "/: Wrong types"))
      (err "/: Wrong types")))

(def * (x y)
  (if (fxp x) (if (fxp y) (fx* x y)
                  (flp y) (fl* (fx->fl x) y)
                  (err "*: Wrong types"))
      (flp x) (if (flp y) (fl* x y)
                  (fxp y) (fl* x (fx->fl y))
                  (err "*: Wrong types"))
      (err "*: Wrong types")))

(def < (x y)
  (if (fxp x) (if (fxp y) (fx< x y)
                  (flp y) (fl< (fx->fl x) y)
                  (err "<: Wrong types"))
      (flp x) (if (flp y) (fl< x y)
                  (fxp y) (fl< x (fx->fl y))
                  (err "<: Wrong types"))
      (err "<: Wrong types")))

(def rem (x y) (fxrem x y))

(def <= (x y)
  (if (fxp x) (if (fxp y) (fx<= x y)
                  (flp y) (fl<= (fx->fl x) y)
                  (err "<=: Wrong types"))
      (flp x) (if (flp y) (fl<= x y)
                  (fxp y) (fl<= x (fx->fl y))
                  (err "<=: Wrong types"))
      (err "<=: Wrong types")))

(def >= (x y)
  (if (fxp x) (if (fxp y) (fx>= x y)
                  (flp y) (fl>= (fx->fl x) y)
                  (err ">=: Wrong types"))
      (flp x) (if (flp y) (fl>= x y)
                  (fxp y) (fl>= x (fx->fl y))
                  (err ">=: Wrong types"))
      (err ">=: Wrong types")))

(def > (x y)
  (if (fxp x) (if (fxp y) (fx< y x)
                  (flp y) (fl< (fx->fl y) x)
                  (err ">: Wrong types"))
      (flp x) (if (flp y) (fl< y x)
                  (fxp y) (fl< y (fx->fl x))
                  (err ">: Wrong types"))
      (err ">: Wrong types")))

(def str-equal-a (a b i n)
  (if (is i n) t
      (is (str-ref a i) (str-ref b i)) (str-equal-a a b (+ i 1) n)))

(def str-equal (a b)
  (if (is (str-len a) (str-len b))
    (str-equal-a a b 0 (str-len a))))

(def iso (a b)
  (if 
    (consp a)
      (if (consp b) 
        (and (iso (car a) (car b)) (iso (cdr a) (cdr b)))
        nil)
    (strp a)
      (if (strp b)
        (str-equal a b)
        nil)
    (flp a)
      (if (flp b)
        (fl= a b))
    (atom a)
      (if (atom b) (is a b) nil)))

(def acons (x) (consp x))

(def car (x) (car x))

(def not (x) (not x))

(def symbolp (x) (symbolp x))

(def map1 (f l)
  (if l (cons (f (car l)) (map1 f (cdr l)))))

(def map (f l . args)
  (if l
    (let res (apply f (car l) (map1 (fn (x) (car x)) args))
      (cons res (apply map f (cdr l) (map1 (fn (x) (cdr x)) args))))))

(def mapl1 (f l)
  (if l (do (f l) (mapl1 f (cdr l)))))

(def reduce (f l val)
  (if l
      (reduce f (cdr l) (f val (car l)))
      val))

(def append (l1 l2)
  (if l1 
      (cons (car l1) (append (cdr l1) l2))
      l2))

(def mem (el l)
  (if l
      (if (is (car l) el)
	  el
	  (mem el (cdr l)))
      nil))

(def assoc-f (l key . args)
  ; same as assoc, but keys in the a-list can be functions to apply to key
  ; and returns element, not cons cell
  (if l
    (withs (k (car (car l))
            test (if (fnp k) (fn (x) (apply k x args)) (fn (x) (is k x))))
	(if (test key)
          (cdr (car l))
          (apply assoc-f (cdr l) key args)))))

(def vector args 
  (let res (mkvec (len args) nil)
    (list-into-vector res args 0)
    res))
    
(def list-into-vector (v l i)
  (if (and l (< i (vec-len v)))
    (do
      (vec-set v i (car l))
      (list-into-vector v (cdr l) (+ i 1)))))

(def copy-vec-aux (from to start end i)
  (if (< start end)
      (do
       (vec-set to i (vec-ref from start))
       (copy-vec-aux from to (+ start 1) end (+ i 1)))
      to))

(def copy-vec (v)
  (let cp (mkvec (vec-len v) nil)
    (copy-vec-aux v cp 0 (vec-len v) 0)))

(def strcopy (from to start end i)
  (if (< start end)
      (do
       (str-set to i (str-ref from start))
       (strcopy from to (+ start 1) end (+ i 1)))
      to))

(def substr (s from to)
  (let res (mkstr (- to from))
    (strcopy s res from to 0)))

(def str-append (s1 s2)
  (let res (mkstr (+ (str-len s1) (str-len s2)))
    (strcopy s1 res 0 (str-len s1) 0)
    (strcopy s2 res (str-len s1) (str-len res) 0)
    res))

; stream: (list fd ungetted-chars)

(def open-file (path type)
  (if (not (strp path)) (err "Invalid path"))
  (withs (flags (if (is type 'in) 0
                    (is type 'out) 577
                    (err "Invalid type passed to open-file"))
          fd (ffi-call "a_open" path flags))
    (if (is fd -1)
      (do (print path) (err (make-string "Cannot open file " path)))
      (list fd nil))))

(def write-string (s stream)
  (ffi-call "a_write" (stream-fd stream) s (str-len s)))

(def stdin-stream ()
  (list 0 nil))

(def stdout-stream ()
  (list 1 nil))

(def stream-fd (s)
  (car s))

(def strstream/in (s)
  (list s 0))

(def readc (s)
  (if (strp (car s)) ; string stream?
    (do (setcar (cdr s) (+ (cadr s) 1))
        (str-ref (car s) (- (cadr s) 1)))
    (if (cadr s) ; there are ungetted chars
      (let c (car (cadr s))
        (setcar (cdr s) (cdr (cadr s))) ; set to next element in list
        c)
      (let val (mkstr 1)
        (let res (ffi-call "a_read" (stream-fd s) val 1)
          (if (is res 0) 
            nil
            (str-ref val 0)))))))

(def ungetc (s c)
  (if (strp (car s)) ; string stream?
    (setcar (cdr s) (- (cadr s) 1)) ; c is ignored...
    (setcar (cdr s) (cons c (cadr s))))) ; append to head of ungetted chars

(def fwhile (test f)
  (if (test)
    (do (f) (fwhile test f))))

(def feach1 (f l)
  (if l
    (do (f (car l)) (feach f (cdr l)))))

(def feach (f l . args)
  (if l
    (let res (apply f (car l) (map1 (fn (x) (car x)) args))
      (apply feach f (cdr l) (map1 (fn (x) (cdr x)) args)))))

(def plist-set (sym k v)
  (let c (assoc k (get-symbol-plist sym))
    (if c 
      (do 
        (setcdr c v) 
        v)
      (do 
        (set-symbol-plist sym (cons (cons k v) (get-symbol-plist sym)))
        v))))

(def plist-get (sym k)
  (cdr (assoc k (get-symbol-plist sym))))

(def butlast (l)
  (if (and l (cdr l) (consp (cdr l)))
    (cons (car l) (butlast (cdr l)))
    nil))
;    (if l (cons (car l) nil) nil)))

(def last (l)
  (if l (if (or (not (cdr l)) (not (consp (cdr l)))) l (last (cdr l)))))

(def trace (name)
  (let f (get-symbol-value name)
    (set-symbol-value name (fn args 
                             (print (make-string "calling " 
                                                 (to-string name) " with:"))
                             (print args)
                             (let res (apply f args)
                               (print (make-string "res: " (to-string res)))
                               res)))))
(def system (cmd)
  (if (not (strp cmd)) (err "system accepts only strings"))
  (ffi-call "a_system" cmd))

(let next-sym 0
  (def uniq ()
    (set next-sym (+ next-sym 1))
    (intern (make-string "gs" next-sym))))

(def read-from-string (s)
  (let in (strstream/in s)
    (read/tbl in read-table)))

(def eval (expr)
  (let out-name (make-string "tmp/" (uniq) ".s")
    (with (out (open-file out-name 'out)
           old stdout*)
      (set stdout* out)
      (emit-unit expr (mk-empty-env) transform-expr)
      (set stdout* old)
      (if (is (system (make-string "gcc --shared -o " out-name ".so " 
                                   out-name " 2>err")) 0)
        (__load (make-string out-name ".so"))
        (err "Compilation error: see file err")))))

(def ccc (f)
  (__ccc (fn (cc) (f [__restore-continuation cc _]))))

; compile and load a file
(def load (file-name)
  (withs (out-name (make-string "tmp/to-load-" (uniq) ".s")
          so-name (make-string out-name ".so")
          in-file (open-file file-name 'in)
          out-file (open-file out-name 'out))
    (compile in-file out-file nil transform-expr)
    (if (is (system (make-string "gcc --shared -o " so-name " " out-name " 2>err")) 0)
      (__load so-name)
      (err "Compilation error: see file err"))))
