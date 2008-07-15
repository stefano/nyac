(__load "basic-fns.arc.so")
(__load "basic-macs.arc.so")
(__load "lib.arc.so")
(__load "print.arc.so")
(__load "read.arc.so")
(__load "comp-utils.arc.so")
(__load "code-walker.arc.so")
(__load "transformations.arc.so")
(__load "comp.arc.so")

(set out-dir (to-string (read/tbl (stdin-stream) read-table)))
(compile-minimal "std.arc" out-dir)
(feach (fn (f) (compile-unit f out-dir)) 
  '("basic-fns.arc" "basic-macs.arc" "print.arc" "read.arc" "lib.arc" 
    "comp-utils.arc" "code-walker.arc" "transformations.arc" 
    "comp.arc"))
(compile-program "test.arc" out-dir)
(compile-program "repl.arc" out-dir)
