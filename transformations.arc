;;;;  Copyright (c) 2008 Dissegna Stefano
;;;;  Released under the terms of the GNU LGPL

(def make-const-table () (cons (list) nil))

(def const-table-expand (tb elem expr is-const)
  ; returns label
  (let lbl (car (cdr (assoc elem (car tb))))
    (if lbl
	lbl
	(let lbl (intern (unique-label))
	  (setcar tb (cons 
                       (cons elem
                             (list lbl (if is-const (list 'const expr) expr)))
                       (car tb)))
	  lbl))))

(def const-table->labels (tb)
  (rev (map (fn (x) (cdr x)) (car tb))))

(def const-string-p (expr)
  (strp expr))

(def expand-const-string (expr)
  (cons 'string (expand-const-string-a expr 0)))

(def expand-const-string-a (expr i)
  (if (< i (str-len expr))
    (cons (str-ref expr i) (expand-const-string-a expr (+ i 1)))))

(def const-vector-p (expr)
  (vecp expr))

(def expand-const-vector (expr ctb)
  (cons 'vector (expand-const-vector-a expr ctb 0)))

(def expand-const-vector-a (expr ctb i)
  (if (< i (vec-len expr))
    (let e (const-expand-all (vec-ref expr i) ctb)
      (cons e (expand-const-vector-a expr ctb (+ i 1))))))

(def const-float-p (expr)
  (flp expr))

(def expand-const-float (expr ctb)
  (const-table-expand ctb expr (list 'float expr) t))

(def quotep (expr)
  (and (consp expr) (is (car expr) 'quote)))

(def quote-expr (expr)
  (cadr expr))

(def expand-const-list (expr ctb)
  (if expr
      (list 'cons (const-expand-all (car expr) ctb) 
	    (const-expand-all (cdr expr) ctb))
      'nil))

(def expand-const-symbol (expr ctb)
  (const-table-expand ctb expr 
		      (list 'funcall 'intern 
			    (expand-const-string (to-string expr))) t))

(def const-expand-all (expr ctb)
  (if 
    (strp expr) (expand-const-string expr)
    (vecp expr) (expand-const-vector expr ctb)
    (const-float-p expr) (expand-const-float expr ctb)
    (is expr nil) 'nil
    (or (is expr 'nil) (is expr 't)) expr
    (symbolp expr) (expand-const-symbol expr ctb)
    (atom expr) expr ; generic atom
    (consp expr) (expand-const-list expr ctb)))

;; lambda and def forms transformations

(def lambdap (expr)
  (if (and (consp expr) (is (car expr) lambda-sym))
    (do
      (if (not (is (len expr) 3))
        (err "Wrong number of arguments to lambda"))
      (if (some (compose not symbolp) (lambda-args-list expr))
        (err (make-string "Malformed lambda expression " expr)))
      t)
    nil))

(def lambda-args (expr)
  (cadr expr))

(def lambda-args-list (expr)
  ; returns proper list of lambda args (including rest arg if present)
  (code-args expr)) ; lambda has the same args format as code forms

(def lambda-has-rest (expr)
  (code-args-has-rest expr))

(def lambda-body (expr)
  (caddr expr))

;; named-lambda form

(def named-lambdap (e)
  (if (and (consp e) (is (car e) 'named-lambda))
    (do
      (if (not (is (len e) 4))
        (err "Wrong number of arguments to named-lambda"))
      t)))

(def named-lambda-name (e)
  (cadr e))

(def named-lambda-args (e)
  (caddr e))

(def named-lambda-body (e)
  (cadddr e))

(def named->lambda (e)
  (list lambda-sym (named-lambda-args e) (named-lambda-body e)))

(def mark-shadowed (sym)
  (add-setted-sym sym)
  (plist-set sym 'shadowed t))

(def shadowed-p (sym)
  (plist-get sym 'shadowed))

(def to-shadow (sym)
  (and (settable-var-p sym) (not (shadowed-p sym))))

(def shadow-vars (names expr)
  (let names (keep to-shadow names)
    (if names
	(list let-sym (map (fn (x) (mark-shadowed x)
				   (list x (list 'cons x nil)))
			   names)
	      (expand-settable-access names expr))
	expr)))

(def expand-settable-read (names expr)
  (transform (fn (e)
	       (if (and (settable-var-p e) (mem e names))
	         (list (list 'car e) t)
		 (list e nil)))
	     expr))

(def expand-settable-write (names expr)
  (transform (fn (e)
	       (if (and (setq-p e) (settable-var-p (setq-var e))
                        (mem (setq-var e) names))
                 (list (list 'setcar (setq-var e) (setq-val e)) nil)
		 (list e nil)))
	     expr))

(def expand-settable-access (names e)
  (expand-settable-write names (expand-settable-read names e)))

(def expand-settables (expr)
  (mark-free-settable expr)
  (transform (fn (e)
	       (if
                 (letp e)
		   (list (list let-sym (let-bindings e)
			   (shadow-vars (let-names e) (let-body e)))
                         nil)
		 (lambdap e)
		   (list (list lambda-sym (lambda-args e)
			   (shadow-vars (lambda-args-list e) (lambda-body e)))
                         nil)
		 (list e nil)))
	      expr))

(set setted-syms* nil)

(def add-setted-sym (s)
  (if (not (mem s setted-syms*))
    (set setted-syms* (cons s setted-syms*))))

(def mark-global (sym)
  (add-setted-sym sym)
  (plist-set sym 'global t))

(def global-var-p (sym)
  (and (symbolp sym) (plist-get sym 'global)))

(def mark-free (sym)
  (add-setted-sym sym)
  (plist-set sym 'free t))

(def free-var-p (sym)
  (and (symbolp sym) (plist-get sym 'free)))

(def mark-settable (sym)
  (add-setted-sym sym)
  (plist-set sym 'settable t))

(def settable-var-p (sym)
  (and (symbolp sym) (plist-get sym 'settable)))

(def builtin-global-p (s)
  (or (mem s (list if-sym let-sym 'do 'ffi-call 
		   setq-sym 'funcall 'apply '__ccc lambda-sym 'intern
                   '__error_continuation))
      (primitivep s)))

(def mark-vars (expr)
  (walk (fn (e dv lv acc)
	  (if (and (varp e) (mem e dv) (not (mem e lv)))
	    (mark-free e))
	  (if (and (varp e) (not (mem e dv)) (not (builtin-global-p e)))
            (mark-global e))
          nil)
        expr))

(def mark-free-settable (expr)
  (walk (fn (e dv lv acc)
          (if (and (setq-p e) (free-var-p (setq-var e)))
	    (mark-settable (setq-var e)))
	  nil)
	expr))

(def reset-marks ()
  (feach (fn (s) (plist-set s 'global nil)
                 (plist-set s 'free nil)
                 (plist-set s 'settable nil)
                 (plist-set s 'shadowed nil))
         setted-syms*)
  (set setted-syms* nil))

(reset-marks)

(def make-free-var-list (expr)
  ; looks in body for references to free variables, return a list of free vars
  (remove-duplicates
   (walk (fn (e dv lv acc)
           (if (and (free-var-p e) (not (mem e dv)))
             (list e)
             nil))
	 expr)))

(def expand-expr-and-collect (expr ctb)
  ; searches constant expressions, substitutes them with an unique label, 
  ; creates expressions for creating the constant values and returns the new 
  ; expression and a list of (label expression).
  ; Substitutes lambda expressions with (closure ...) and collects (code ...)
  (if
    (nullp expr) nil
    (const-string-p expr)
      (const-table-expand ctb expr (expand-const-string expr) t)
    (const-vector-p expr)
      (const-table-expand ctb expr (expand-const-vector expr ctb) t)
    (const-float-p expr)
      (const-table-expand ctb expr (expand-const-float expr ctb) t)
    (quotep expr)
      (const-table-expand ctb (quote-expr expr)  
	                  (const-expand-all (quote-expr expr) ctb) t)
    (ffi-p expr)
     (let e (map (fn (x) (expand-expr-and-collect x ctb)) (cddr expr))
       (append (list 'ffi-call (cadr expr)) e))
    (lambdap expr)
      (err "Found lambda expression at constant value raising")
    (named-lambdap expr)
      (withs (free-vars (make-free-var-list expr)
	      e (expand-expr-and-collect (named-lambda-body expr) ctb)
	      name-expr (expand-expr-and-collect (named-lambda-name expr) ctb)
	      lbl (const-table-expand ctb expr
				      (list 'code (named-lambda-args expr) 
				             free-vars e) nil))
        (append (list 'closure lbl name-expr) free-vars))
    (consp expr)
      (map (fn (x) (expand-expr-and-collect x ctb)) expr)
    expr))

(def lift-const-expressions (expr)
 ; puts constant expressions at the toplevel in a labels form
 (withs (ctb (make-const-table)
	 expr (expand-expr-and-collect expr ctb)
	 to-add (const-table->labels ctb))
   (if (labelsp expr)
       (list 'labels 
	     (append to-add (labels-bindings expr)) (labels-body expr))
       (list 'labels to-add expr))))

; alpha-conversion

(set *sym-count* 0)
(def unique-name ()
  (set *sym-count* (+ *sym-count* 1))
  (intern (str-append "__%%sym_" (to-string *sym-count*))))

(def unique-names (n)
  (if (is n 0) nil (cons (unique-name) (unique-names (- n 1)))))

(def a-conversion (expr transformations)
  (if
    (atom expr)
      (let new-name (cdr (assoc expr transformations))
        (if new-name new-name expr))
    (quotep expr) expr
    (letp expr)
      (let new-names (unique-names (len (let-bindings expr)))
       (list let-sym (map (fn (n bnd)
			    (list n (a-conversion (cadr bnd) transformations)))
		           new-names (let-bindings expr))
	     (a-conversion (let-body expr) 
			   (append (map (fn (x y) (cons x y)) 
                                        (map binding-name (let-bindings expr))
                                        new-names)
				   transformations))))
    (lambdap expr)
      (withs (new-args (unique-names (len (lambda-args-list expr)))
	      l-args (if (lambda-has-rest expr)
		       (append (butlast new-args) (car (last new-args)))
                       new-args))
        (list lambda-sym l-args
	      (a-conversion (lambda-body expr)
		            (append (map (fn (x y) (cons x y))
                                         (lambda-args-list expr) 
                                         new-args)
				    transformations))))
    (consp expr)
      (map (fn (x) (a-conversion x transformations)) expr)))

(def expand-global-ref (e)
  (transform (fn (x) 
	       (if
	         (global-var-p x) (list 
                                    (list 'get-symbol-value (list 'quote x)) 
                                    nil)
		 (and (setq-p x) (global-var-p (setq-var x)))
		   (list (list 'set-symbol-value (list 'quote (setq-var x)) 
                           (setq-val x))
                         nil)
		 (and x (consp x) (not (quotep x)))
		   (let res (cons (car x) 
				  (map (fn (x) 
				         (if (primitivep x)
					   (list 'get-symbol-value 
                                                 (list 'quote x))
                                           x))
				       (cdr x)))
		      (list res nil))
		 (list x nil)))
	     e))

(def expand-fun-app (e)
  (transform (fn (x) 
	       (if (and (consp x) (> (len x) 0) (not (syntax-form-p x)))
                 (list (cons 'funcall x) nil) 
		 (list x nil)))
	     e))

(def lift-labels (expr)
  (if (and (do-p expr) (labelsp (cadr expr))) (cadr expr) expr))

(def name-lambdas (e)
  ; transforms lambdas in named lambdas to aid debugging
  (transform (fn (x)
	       (if
	         (lambdap x)
		   (list
		     (list 'named-lambda 
			   (list 'quote (cons lambda-sym (lambda-args x))) 
			   (lambda-args x)
			   (name-lambdas (lambda-body x))) t)
		 (and (consp x) (is (car x) 'set-symbol-value) 
                      (lambdap (caddr x)))
		   (list
		     (list 'set-symbol-value (cadr x)
		           (list 'named-lambda (cadr x) 
			         (lambda-args (caddr x))
			         (name-lambdas (lambda-body (caddr x)))))
		     t)
		 (list x nil)))
	     e))

(def transform-expr (expr)
  (withs (expr (expand-all expr)
	  expr (a-conversion expr nil)
	  expr (expand-fun-app expr))
    (reset-marks)
    (mark-vars expr)
    (withs (expr (expand-settables expr)
	    expr (expand-global-ref expr)
	    expr (name-lambdas expr)
	    expr (lift-labels expr)
	    expr (lift-const-expressions expr))
      expr)))

(def minimal-transform (e)
  (withs (;e (expand-all e)
	  e (lift-labels e)
	  e (lift-const-expressions e))
    e))

(def expand-all (expr)
  (transform (fn (e)
               (list
	         (if (consp e)
	           (let m (assoc (car e) macros*)
                     (if m
 		       (expand-all (apply (cdr m) (cdr e)))
                       e))
                   e)
                 nil))
             expr))

; installs a macro (not a true transformation)
(install-macro 'mac 
  (fn (name args . body)
    ;(install-macro name (eval (list '__fn args (cons 'do body))))
    (list 'install-macro (list 'quote name) (list '__fn args (cons 'do body)))))
