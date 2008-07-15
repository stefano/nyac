(__load "basic-fns.l.so")
(__load "basic-macs.l.so")
;(__load "lib.l.so")
;(__load "print.l.so")
;(__load "read.l.so")
;(__load "meta-comp-utils.l.so")
;(__load "meta-code-walker.l.so")
;(__load "meta-transformations.l.so")
;(__load "meta-comp.l.so")


;(compile-file (to-string (read/tbl (stdin-stream) read-table)))

;(with (in (stdin-stream)
;       out (stdout-stream))
;  (def repl ()
;    (write-string "> " out)
;    (print (eval (read/tbl in read-table)))
;    (repl)))

;(repl)

9
