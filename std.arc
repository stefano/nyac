;  Copyright (c) 2008 Dissegna Stefano
;  Released under the terms of the GNU LGPL

(labels ((__error 
	   (code (msg) ()
	     (funcall __error_continuation msg)))

	 (__type_error 
	   (code () ()
	     (labelcall __error "Wrong type
")))
	 
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

         (__print_and_exit
           (code (msg) ()
             (ffi-call "a_write" 2 msg (str-len msg))
	     (__print_backtrace)
	     (ffi-call "exit" 1)))
         
         ; function called whenever an error occurs
         ; !! it _must_ never return because because error checking
         ; !! code emitted by the compiler makes direct jumps to the
         ; !! error label, so no return is possible.
         (__error_continuation (const (closure __print_and_exit "__exit")))))
