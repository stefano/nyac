(__load "build-arc/basic-fns.arc.so")
(__load "build-arc/basic-macs.arc.so")
(__load "build-arc/lib.arc.so")
(__load "build-arc/print.arc.so")
(__load "build-arc/read.arc.so")
(__load "build-arc/comp-utils.arc.so")
(__load "build-arc/code-walker.arc.so")
(__load "build-arc/transformations.arc.so")
(__load "build-arc/comp.arc.so")

(compile-minimal "std.arc" "build-arc")
(feach (fn (f) (compile-unit f "build-arc")) 
  '("basic-fns.arc" "basic-macs.arc" "print.arc" "read.arc" "lib.arc" 
    "comp-utils.arc" "code-walker.arc" "transformations.arc" 
    "comp.arc"))
(compile-program "test.arc" "build-arc")
(compile-program "repl.arc" "build-arc")
