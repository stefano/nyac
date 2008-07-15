(__load "build-arc/basic-fns.arc.so")
(__load "build-arc/basic-macs.arc.so")
(__load "build-arc/lib.arc.so")
(__load "build-arc/print.arc.so")
(__load "build-arc/read.arc.so")
(__load "build-arc/comp-utils.arc.so")
(__load "build-arc/code-walker.arc.so")
(__load "build-arc/transformations.arc.so")
(__load "build-arc/comp.arc.so")

(set out-dir (to-string (read/tbl (stdin-stream) read-table)))
(compile-minimal "std.arc" out-dir)
(feach (fn (f) (compile-unit f out-dir)) 
  '("basic-fns.arc" "basic-macs.arc" "print.arc" "read.arc" "lib.arc" 
    "comp-utils.arc" "code-walker.arc" "transformations.arc" 
    "comp.arc"))
(compile-program "test.arc" out-dir)
(compile-program "repl.arc" out-dir)
