; functions needed by basic-macs.arc

(set-symbol-value 'err 
  (__fn (msg)
    (__if __error_continuation
      (__error_continuation msg)
      (do
        (ffi-call "a_write" 1 msg (str-len msg))
        (ffi-call "a_write" 1 "
" 1)
        (ffi-call "exit" 1)))))

(set-symbol-value 'list (__fn elems elems))

(set-symbol-value 'caar (__fn (l) (car (car l))))

(set-symbol-value 'cadr (__fn (l) (car (cdr l))))

(set-symbol-value 'caddr (__fn (l) (car (cdr (cdr l)))))

(set-symbol-value 'cadddr (__fn (l) (car (cdr (cdr (cdr l))))))

(set-symbol-value 'cddr (__fn (l) (cdr (cdr l))))

(set-symbol-value 'cdddr (__fn (l) (cdr (cdr (cdr l)))))

(set-symbol-value 'caadr (__fn (l) (car (car (cdr l)))))

(set-symbol-value 'len 
  (__fn (l)
    (__if l (fx+ (len (cdr l)) 1) 0)))

(set-symbol-value 'rev-a 
  (__fn (l acc)
    (__if l (rev-a (cdr l) (cons (car l) acc)) acc)))

(set-symbol-value 'rev 
  (__fn (l)
    (rev-a l nil)))

(set-symbol-value 'list-of-n 
  (__fn (l n acc)
    ; returns a cons with a list containing the first n elements of l
    ; and the rest of l
    (__if (__if (not l) t (is n 0))
      (cons (rev acc) l)
      (list-of-n (cdr l) (fx- n 1) (cons (car l) acc)))))

(set-symbol-value 'group-n 
  (__fn (l n)
    ; groups elements of l into groups of n el.
    (__if l
      (__let ((res (list-of-n l n nil)))
        (cons (car res) (group-n (cdr res) n)))
      nil)))

(set-symbol-value 'assoc 
  (__fn (key l)
    (__if l
      (__if (is (car (car l)) key)
        (car l)
        (assoc key (cdr l))))))

(set macros* (list))

(set-symbol-value 'install-macro 
  (__fn (name fn)
    (__let ((cell (assoc name macros*)))
      (__if cell
        (setcdr cell fn)
        (set macros* (cons (cons name fn) macros*))))))

(set list-ref 
  (__fn (l n)
    (__if (is n 0) (car l) (list-ref (cdr l) (fx- n 1)))))

; split the argument list of a (fn (args) ...) form into two lists:
; the first contains the first atomic symbols, the second the rest of 
; the argument list
(set split-arglist
  (__fn (l acc)
    (__if (symbolp l) ; arg list with rest parameter
      (list (rev (cons l acc)) nil)
      (__let ((arg (car l)))
        (__if (__if (not l) t (consp arg))
          (list (rev acc) l)
          (split-arglist (cdr l) (cons arg acc)))))))

; tells if l is a list ending with nil
(set proper-list
  (__fn (l)
    (__if (not l) t (__if (consp l) (proper-list (cdr l)) nil))))

; set the value of the last cdr in a list
(set set-last-cdr!
  (__fn (l val)
    (__if l
      (__if (consp (cdr l))
        (set-last-cdr! (cdr l) val)
        (setcdr l val)))))
