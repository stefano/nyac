; macros needed to run the meta compiler

(mac or args
  (__if (nullp args) 
    nil
    (__if (is (len args) 1) 
      (car args)
      (__let ((s (uniq)))
        (list '__let (list (list s (car args)))
          (list '__if s s (cons 'or (cdr args))))))))

(mac and args
  (__if (nullp args) 
    t
    (__if (is (len args) 1) 
      (car args)
      (list '__if (car args) (cons 'and (cdr args)) nil))))

(mac if args
  ;(__if (fx< (len args) 2)
  ;  (err "Malformed if expression"))
  (__if (or (nullp args) (is (len args) 1))
    (car args)
    (__let ((test (car args))
            (action (car (cdr args)))
            (rest (cdr (cdr args))))
      (list '__if test action (cons 'if rest)))))
 
(mac __let* (bnds body)
  (if bnds
    (list '__let (list (car bnds)) 
      (list '__let* (cdr bnds) body))
    body))
 
(mac let (name val . body)
  (list '__let (list (list name val)) (cons 'do body)))

(mac with (bnds . body)
  (list '__let (group-n bnds 2) (cons 'do body)))

(mac withs (bnds . body)
  (list '__let* (group-n bnds 2) (cons 'do body)))

(mac fn (args . body)
  (list '__fn args (cons 'do body)))

(mac def args
  (if (fx>= (len args) 3) (list 'set-symbol-value (list 'quote (car args)) 
	                    (cons 'fn (cons (cadr args) (cddr args))))
      (is (len args) 2) (list 'set-symbol-value (list 'quote (car args))
                               (cadr args))
      (err "Wrong number of arguments passed to def")))
