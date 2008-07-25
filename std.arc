;  Copyright (c) 2008 Dissegna Stefano
;  Released under the terms of the GNU LGPL

(labels ((__print_strings ; prints all strings passed to stderr
           (code strings ()
             (__if strings
               (do
                 (ffi-call "a_write" 2 (car strings) (str-len (car strings)))
	         (labelcall __print_strings (cdr strings))))))

         ; converts between type tags and type names
         ; 0, 4 -> fixnum
         ; 1 -> cons
         ; 2 -> function
         ; 3 -> symbol
         ; 5 -> vector
         ; 6 -> string
         ; 15 -> character
         ; 47 -> nil
         ; 111 -> t
         ; 175 -> continuation
         ; 207 -> float
         (__type_to_name 
           (code (tag) ()
             (__if (__if (is tag 0) t (is tag 4)) "fixnum"
               (__if (is tag 1) "cons"
                 (__if (is tag 2) "function"
                   (__if (is tag 3) "symbol"
                     (__if (is tag 5) "vector"
                       (__if (is tag 6) "string"
                         (__if (is tag 15) "char"
                           (__if (is tag 47) "nil"
                             (__if (is tag 111) "t"
                               (__if (is tag 175) "continuation"
                                 (__if (is tag 207) "float"
                                   "unknown")))))))))))))

         (__error 
	   (code (msg) ()
             (__if __error_continuation
	       (funcall __error_continuation msg)
               (do ; default handler
                 (ffi-call "a_write" 2 msg (str-len msg))
	         (__print_backtrace)
	         (ffi-call "exit" 1)))))

	 (__type_error
	   (code (expected-tag got-tag) ()
             (__let ((expected (labelcall __type_to_name expected-tag))
                     (got (labelcall __type_to_name got-tag)))
               (__if __error_continuation
                 (funcall __error_continuation (cons expected got))
	         (do
                   (labelcall __print_strings "Wrong type: expected " 
                                              expected ", got " got "
")
                   (__print_backtrace)
                   (ffi-call "exit" 1))))))
	 
	 (__unbound_error
	   (code (s) ()
	     (do
	       (__let ((var (get-symbol-string s)))
	         (ffi-call "a_write" 2 var (str-len var)))
               (labelcall __error " is unbound
"))))

	 (__unbound_error_fun (const (closure __unbound_error "UE")))

	 (__bounds_error
	   (code () ()
	     (labelcall __error "Out of bounds
")))

	 (__arg_count_error 
	   (code () ()
	     (labelcall __error "Wrong number of arguments
")))
	 (__load_error
	   (code () ()
	     (labelcall __error "Load error
")))
	 
	 (__sym_table (const nil))
	 
	 (__string_eq_aux 
	   (code (s1 s2 i1 i2) ()
	     (__if (__if (fx< i1 (str-len s1))
                     (__if (fx< i2 (str-len s2)) t nil) nil)
	       (__if (is (str-ref s1 i1) (str-ref s2 i2))
                 (labelcall __string_eq_aux s1 s2 (fxadd1 i1) (fxadd1 i2))
                 nil)
	       t)))
   
	 (__string_eq 
	   (code (s1 s2) ()
	     (__if (fx= (str-len s1) (str-len s2))
	       (labelcall __string_eq_aux s1 s2 0 0)
	       nil)))

	 (__find_sym 
	   (code (s l) ()
	     (__if l
	       (__if (labelcall __string_eq s (get-symbol-string (car l)))
	         (car l)
		 (labelcall __find_sym s (cdr l))))))

	 (__intern 
	   (code (s) ()
             (__let ((sym (labelcall __find_sym s __sym_table)))
	       (__if (not sym)
	         (__let ((sym (mksymbol s)))
	           (do
	             (set __sym_table (cons sym __sym_table))
		     sym))
                 sym))))

	 (intern (const (closure __intern "intern")))

         ; function called whenever an error occurs
         ; !! it _must_ never return because because error checking
         ; !! code emitted by the compiler makes direct jumps to the
         ; !! error label, so no return is possible.
         (__error_continuation (const nil))))
