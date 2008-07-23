(__load "basic-fns.arc.so")
(__load "basic-macs.arc.so")
(__load "lib.arc.so")
(__load "print.arc.so")
(__load "read.arc.so")
(__load "comp-utils.arc.so")
(__load "code-walker.arc.so")
(__load "transformations.arc.so")
(__load "comp.arc.so")

(with (in (stdin-stream)
       out (stdout-stream))
  (def repl ()
    (ccc [set __error_continuation
             (fn (s)
               (print (make-string "Error: " s))
               ;(__print_backtrace)
               (_ nil))])
    (write-string "arc> " out)
    (print (eval (read/tbl in read-table)))
    (repl)))

(repl)
