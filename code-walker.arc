;;;;  Copyright (c) 2008 Dissegna Stefano
;;;;  Released under the terms of the GNU LGPL

; fun takes 4 args: expr, declared vars, locals vars, list of results returned by previous calls
(def walk (fun code)
  (keep (fn (x) x) (walk-a fun '() '() '() code)))

(def walk-a (fun fun-acc dv lv expr)
  (if
    (quotep expr) nil
    (letp expr)
      (with (names (map binding-name (let-bindings expr))
	     exprs (map binding-expr (let-bindings expr)))
        (let acc (append (mappend (fn (x) (walk-a fun fun-acc dv lv x)) exprs)
			 fun-acc)
          (walk-a fun acc (append names dv) (append names lv) (let-body expr))))
    (lambdap expr)
      (with (dv (append dv (lambda-args-list expr))
	     lv (lambda-args-list expr))
        (walk-a fun (fun (lambda-body expr) dv lv fun-acc) dv lv 
                (lambda-body expr)))
    (named-lambdap expr)
      (walk-a fun fun-acc dv lv (named->lambda expr))
    (consp expr)
      (let acc (fun expr dv lv fun-acc)
        (append (mappend (fn (x) (walk-a fun acc dv lv x)) expr) fun-acc))
    (append (fun expr dv lv fun-acc) fun-acc)))

(def transform (fun expr)
  (withs (res (fun expr)
          e (car res)
          stop (cadr res))
    (let f (fn (x) (transform fun x))
      (if
	(or stop (atom e) (quotep e)) e
	(letp e)
	 (let exprs (map f (let-exprs e))
	   (list let-sym (map list (let-names e) exprs)
                 (transform fun (let-body e))))
	(lambdap e)
	  (list lambda-sym (lambda-args e)
	        (transform fun (lambda-body e)))
	(named-lambdap e) (transform fun (named->lambda e))
	(setq-p e) (list setq-sym (setq-var e) (transform fun (setq-val e)))
	(ffi-p e) (append (list 'ffi-call (ffi-name e)) (map f (ffi-args e)))
	(consp e) (map f e)))))
