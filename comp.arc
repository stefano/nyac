;  Copyright (c) 2008 Dissegna Stefano
;  Released under the terms of the GNU LGPL

; Registers:
; eax: return values, number of arguments before function call
; esp: stack pointer (grows downwards)
; ebp: heap pointer (grows upwards)
; edi: current closure pointer
; esi: structure holding saved C registers

(set stdout* (stdout-stream))

(def emit args
  (write-string (reduce str-append (map to-string args) "") stdout*)
  (write-string "
" stdout*))

(set wordsize 4)

(set fxshift 2)
(set fxtag 0) ; #b00
(set fxmask 3) ; #x03

(set chshift 8)
(set chtag 15) ; #b00001111
(set chmask 255) ; #xFF

(set nil-val 47) ; #x2F
(set t-val 111) ; #x6F
(set bool-bit 6)

(set cellmask 7) ; #x7
(set celltag 1) ; #b001
(set cell-size (* 2 wordsize))
(set car-offset (- celltag))
(set cdr-offset (- wordsize celltag))

(set vecmask 7) ; #x7
(set vectag 5) ; #b101

(set closuremask 7) ; #x7
(set closuretag 2) ; #b010

(set closurelen-offset (- closuretag))
(set closureaddr-offset (- wordsize closuretag))

(set symbolmask 7) ; #x7
(set symboltag 3) ; #b011
(set symbolstring-offset (- symboltag))
(set symbolval-offset (- wordsize symboltag))
(set symbolplist-offset (+ symbolval-offset wordsize))
(set basicmask 7) ; #x7

(set extendedtag 6) ; #b110

(set strtag fxtag)
(set strchar-offset (- wordsize extendedtag))

(set floattag 207) ; #xCF
(set float-offset (- wordsize extendedtag))

(set continuation-tag 175) ; #xAF

(set unbound-val 63) ; #b111111

;; tells that the next value in the stack is a return adress
;; needed by the GC while scanning the stack
(set frame-sentinel 255) ; #xFF

(set fixnum-bits (- (* wordsize 8) fxshift))
(set fxlower -536870912);(- (expt 2 (- fixnum-bits 1))))
(set fxupper (- (- fxlower) 1))

(set let-sym '__let)
(set lambda-sym '__fn)
(set if-sym '__if)
(set setq-sym 'set)

(set call-table-lbl "__call_table")
(set call-table-err-lbl "__call_table_type_error")

(def fixnump (x)
  (fxp x)); (<= fxlower x) (<= x fxupper)))

(def charp (x)
  (charp x))

(def immediatep (x)
  (or (fixnump x) (charp x) (is x nil) (is x t)))

(def imm-rep (x)
  (if 
    (fixnump x) (* x 4);fxshift)
    (charp x) (+ (* (char->fx x) 256) chtag)
    (is x nil) nil-val
    (is x t) t-val
    (is x 't) t-val
    (is x 'nil) nil-val
    (err "Cannot find suitable representation")))

(def install-primop (name f n-args rest-args)
  (plist-set name 'is-prim t)
  (plist-set name 'n-args n-args)
  (plist-set name 'rest-args rest-args)
  (plist-set name 'emitter f))

(def make-alias (name1 name2)
  (plist-set name1 'emitter (plist-get name2 'emitter))
  (plist-set name1 'n-args (plist-get name2 'n-args))
  (plist-set name1 'rest-args (plist-get name2 'rest-args))
  (plist-set name1 'is-prim (plist-get name2 'is-prim)))

(def primitivep (x)
  (and (symbolp x) (plist-get x 'is-prim)))

(def emitter (x)
  (or (plist-get x 'emitter) 
      (err (make-string "Couldn't find an emitter for " x))))

(def primcallp (expr)
  (and (consp expr) (primitivep (car expr))))

(def check-primcall-args (prim args)
  (if (not (or (and (not (plist-get prim 'rest-args))
		    (is (len args) (plist-get prim 'n-args)))
	       (and (plist-get prim 'rest-args)
		    (>= (len args) (plist-get prim 'n-args)))))
      (err (str-append "Wrong number of arguments to " (to-string prim)))))

(def emit-primcall (si env expr)
  (with (prim (car expr) args (cdr expr))
    (check-primcall-args prim args)
    (apply (emitter prim) si env args)))

(set ax "%ax")
(set al "%al")
(set eax "%eax")
(set ebx "%ebx")
(set ecx "%ecx")
(set edx "%edx")
(set edi "%edi")
(set ebp "%ebp")
(set esp "%esp")
(set esi "%esi")

(def make-string args
  (reduce str-append (map to-string args) ""))

(def globl (lbl)
  (emit "    .globl " lbl))

(def imm (val)
  (make-string "$" val))

(def deref (offset reg)
  (make-string offset "(" reg ")"))

(def unref-call (dest)
  (make-string "*" dest))

(def call (label)
  (emit "    call " label))

(def sete (reg)
  (emit "    sete " reg))

(def setl (reg)
  (emit "    setl " reg))

(def setle (reg)
  (emit "    setle " reg))

(def setg (reg)
  (emit "    setg " reg))

(def setge (reg)
  (emit "    setge " reg))

(def seta (reg)
  (emit "    seta " reg))

(def setb (reg)
  (emit "    setb " reg))

(def setbe (reg)
  (emit "    setbe " reg))

(def setae (reg)
  (emit "    setae " reg))

(def movzbl (r1 r2)
  (emit "    movzbl " r1 ", " r2))

(def cmp (a b)
  (emit "    cmpl " a ", " b))

(def op-and (a b)
  (emit "    andl " a ", " b))

(def op-or (a b)
  (emit "    or " a ", " b))

(def op-orl (a b)
  (emit "    orl " a ", " b))

(def andl (a b)
  (emit "    andl " a ", " b))

(def label (lbl)
  (emit lbl ":"))

(def jmp (lbl)
  (emit "    jmp " lbl))

(def je (lbl)
  (emit "    je " lbl))

(def jne (lbl)
  (emit "    jne " lbl))

(def jl (lbl)
  (emit "    jl " lbl))

(def jge (lbl)
  (emit "    jge " lbl))

(def unref (offset reg dest)
  (emit "    movl " offset "(" reg "), "  dest))

(def movb (from to)
  (emit "    movb " from ", " to))

(def movl (from to)
  (emit "    movl " from ", " to))

(def lea (base dest-reg)
  (emit "    lea " base ", " dest-reg))

(def addl (a b)
  (emit "    addl " a ", " b))

(def subl (a b)
  (emit "    subl " a ", " b))

(def sal (a b)
  (emit "    sal " a ", " b))

(def sall (a b)
  (emit "    sall " a ", " b))

(def shll (a b)
  (emit "    shll " a ", " b))

(def shrl (a b)
  (emit "    shrl " a ", " b))

(def sarl (a b)
  (emit "    sarl " a ", " b))

(def xorl (a b)
  (emit "    xorl " a ", " b))

(def imull (a b)
  (emit "    imull " a ", " b))

(def idivl (a)
  (emit "    idivl " a))

(def cdq ()
  (emit "    cdq"))

(def emit-save (si reg)
  ; emit code for saving reg in the stack
  (emit "    movl " reg ", " si "(" esp ")"))

(def emit-load (si reg)
  (emit "    movl " si "(" esp "), " reg))

(def pushl (what)
  (emit "    pushl " what))

(def next-si (si)
  (- si wordsize))

(def next-si-n (si n)
  (- si (* n wordsize)))

(def fstpl (from)
  (emit "    fstpl " from))

(def fldl-plain (arg)
  (emit "    fldl " arg))

(def fucompp ()
  (emit "    fucompp"))

(def fnstsw (reg)
  (emit "    fnstsw " reg))

(def sahf ()
  (emit "    sahf"))

; the following float operations assume reg is a register containing the
; adress of a tagged float object and do automatic dereferencing
(def fldl (reg)
  (emit "    fldl " float-offset "(" reg ")"))

(def faddl (reg)
  (emit "    faddl " float-offset "(" reg ")"))

(def fsubl (reg)
  (emit "    fsubl " float-offset "(" reg ")"))

(def fmull (reg)
  (emit "    fmull " float-offset "(" reg ")"))

(def fdivl (reg)
  (emit "    fdivl " float-offset "(" reg ")"))

(def fildl (si)
  (emit "    fildl " si "(" esp ")"))

(def fistpl (si)
  (emit "    fistpl " si "(" esp ")"))

(def emit-call-expand-heap (si . size)
  ; expects number of bytes to allocate in %eax if size is nil
  ; address of allocated memory will be in %eax
  (let size (car size)
    (emit-save si edi) ; do not collect cl. pt!
    (let si (next-si si)
      (if size
        (movl (imm size) (deref si esp))
        (movl eax (deref si esp)))
      (lea (deref si esp) eax)
      (movl eax (deref (next-si si) esp))
      ;(movl ebp (deref (next-si-n si 2) esp))
      (addl (imm (next-si si)) esp)
      ;(addl (imm (next-si-n si 2)) esp)
      (call "main_expand_heap2")
      (subl (imm (next-si si)) esp))
      ;(subl (imm (next-si-n si 2)) esp)
      ;(movl eax ebp))
    (emit-load si edi)))

; runtime global memory area holding the base of the current stack
(set main-stack-base "main_stack_base")

;(def emit-stack-copy-rev (si)
  ; emit code to copy the stack up to si (exclusive) into the heap
  ; the stack grows downwards and the heap upwards, so it is copied in 
  ; reverse order
  ; space on the heap must be allocated and memory pointer must be in %ebp
  ; ebx: source pointer
  ; ebp: target pointer
;  (movl main-stack-base ebx)
;  (addl (imm si) esp) ; esp: first adress not to copy
;  (with (loop-start (unique-label)
;         end (unique-label))
;    (label loop-start)
;    (cmp ebx esp)
;    (je end)
;    (movl (deref 0 ebx) ecx) ; copy wordsize bytes
;    (movl ecx (deref 0 ebp))
;    (addl (imm wordsize) ebp) ; next target adress
;    (addl (imm (- wordsize)) ebx) ; next source adress
;    (jmp loop-start)
;    (label loop-end))
;  (subl (imm si) esp)) ; restore esp

(def emit-restore-stack ()
  ; emit code to copy back the stack from the heap to main_stack_base
  ; expects closure in eax and continuation's stack pt. in esp
  ; ecx: current address to copy
  ; edx: main stack destination
  ; esp: destination end point
  (movl (deref (+ (- extendedtag) wordsize) eax) ebx) ; stack len
  (lea (deref (+ (- extendedtag) (* 5 wordsize)) eax) ecx) ; stack start addr.
  (movl main-stack-base edx)
  (movl edx esp)
  (subl ebx esp)
  (subl (imm wordsize) edx)
  (with (loop-start (unique-label)
         loop-end (unique-label))
    (label loop-start)
    (cmp edx esp) ; test 
    (je loop-end)
    (movl (deref 0 ecx) ebx)
    (movl ebx (deref 0 edx))
    (addl (imm wordsize) ecx)
    (addl (imm (- wordsize)) edx)
    (jmp loop-start)
    (label loop-end)))
;  (movl (deref (+ (- extendedtag) wordsize) eax) ebx) ; stack len
;  (addl ebx esp))

(def emit-call-stack-copy-rev (si)
  ; emit code to call the C function stack_copy_rev
  ; stack_copy_rev allocates heap space, so edi must be passed in the stack
  ; and then restored, because the GC could change the location of the closure
  ; it points at
  (emit-save si edi)
  (let si (next-si si)
    (lea (deref si esp) eax) ; stack_top
    (emit-save si eax)
    (addl (imm si) esp)
    (call "stack_copy_rev")
    (subl (imm si) esp))
  (emit-load si edi))

(def emit-type-pred (basic-mask basic-tag . rest)
  (with (etag (car rest)
         emask (cadr rest))
    (movl eax ebx)
    (andl (imm basic-mask) eax)
    (cmp (imm basic-tag) eax)
    (if etag
      (let end (unique-label)
        (jne end)
        (unref (- extendedtag) ebx ebx)
        (if emask
	  (op-and (imm emask) ebx))
        (cmp (imm etag) ebx)
        (label end)))
    (sete al)
    (movzbl al eax)
    (sal (imm bool-bit) al)
    (op-or (imm nil-val) al)))

(def emit-type-check (si env name);mask tag)
  ; emit code fore checking type of value in %eax
  ;(movl eax ebx)
  ;(op-and (imm mask) ebx)
  ;(cmp (imm tag) ebx)
  ;(let cont-label (unique-label)
  ;  (je cont-label)
  ;  (movl (imm 0) eax)
  ;  (addl (imm si) esp)
  ;  (jmp '__type_error)
  ;  (label cont-label)))
  (addl (imm (+ si wordsize)) esp)
  (call (make-string "__check_" name))
  (subl (imm (+ si wordsize)) esp))

(def emit-get-tag (src dest-reg tmp-reg)
  ; get the tag of the object pointed by the register src
  ; put tag in dest-reg
  (let cont (unique-label) ; label to the end of this routine
    ; check if it is a character
    (movl src dest-reg)
    (op-and (imm chmask) dest-reg)
    (cmp (imm chtag) dest-reg)
    (je cont) ; yes it is
    ; check if it is a basic type
    (movl src dest-reg)
    (op-and (imm basicmask) dest-reg)
    (cmp (imm extendedtag) dest-reg) ; is it an extended type?
    (jne cont)
    (movl (deref (- extendedtag) src) dest-reg) ; get extended type tag
    ; special case to handle strings' tag
    (movl dest-reg tmp-reg) 
    (op-and (imm fxmask) tmp-reg)
    (cmp (imm strtag) tmp-reg)
    (jne cont)
    (movl (imm extendedtag) dest-reg) ; it's a string
    (label cont)))

(def emit-static-type-check-routine (name mask tag extended-p)
  (decl-globl (make-string "__check_" name))
  (emit-fun-header (make-string "__check_" name))
  (movl eax ebx)
  (op-and (imm (if extended-p basicmask mask)) ebx)
  (cmp (imm (if extended-p extendedtag tag)) ebx)
  (let err-label (unique-label)
    (jne err-label)
    (if extended-p
      (do
        (movl (deref (- extendedtag) eax) ebx) ; get extended object tag
        (if mask
          (op-and (imm mask) ebx))
        (cmp (imm tag) ebx)
        (jne err-label)))
    (emit-fun-ret)
    (label err-label)
    (subl (imm wordsize) esp) ; adjust esp to be consistent with labelcall
    (emit-save wordsize (imm frame-sentinel))
    (emit-save 0 (imm 0)) ; won't return, no need to have a valid ret. point
    (if (and extended-p mask)
      (movl (imm (+ extendedtag tag)) ecx)
      (movl (imm tag) ecx))
    (shll (imm fxshift) ecx) ; make the tag a fixnum
    (emit-save (next-si 0) ecx) ; pass expected tag
    (emit-get-tag eax ebx ecx)
    (shll (imm fxshift) ebx)
    (emit-save (next-si-n 0 2) ebx) ; pass tag found
    (movl (imm 2) eax) ; number of args passed
    (jmp '__type_error)))

(def emit-extended-type-check (si env tag . mask)
  (let mask (car mask)
    (emit-type-check si env "extended"); basicmask extendedtag)
    (movl (deref (- extendedtag) eax) ebx)
    (if mask
      (op-and (imm mask) ebx))
    (cmp (imm tag) ebx)
    (with (;error-label (unique-label)
	   cont-label (unique-label))
      ;(jne error-label)
      ;(jmp cont-label)
      (je cont-label)
      ;(label error-label)
      (movl (imm 0) eax)
      (addl (imm si) esp)
      (jmp '__type_error)
      (label cont-label))))

(def emit-is-fx (si env)
  (emit-type-check si env "fx"));fxmask fxtag))

(def emit-is-ch (si env)
  (emit-type-check si env "ch"));chmask chtag))

(def emit-is-cell (si env)
  (emit-type-check si env "cell"));cellmask celltag))

(def emit-is-vec (si env)
  (emit-type-check si env "vec"));vecmask vectag))

(def emit-is-str (si env)
  (emit-type-check si env "str"));strtag fxmask))

(def emit-is-float (si env)
  (emit-type-check si env "float"));floattag))

(def emit-is-sym (si env)
  (emit-type-check si env "sym"));symbolmask symboltag))

(def emit-is-closure (si env)
  (emit-type-check si env "closure"));closuremask closuretag))

(def emit-is-continuation (si env)
  (emit-type-check si env "continuation"))

(def emit-exact-arg-count-check (si env n)
  (with (error-label (unique-label)
	 cont-label (unique-label))
    (cmp (imm n) eax)
    ;(jne error-label)
    ;(jmp cont-label)
    (je cont-label)
    ;(label error-label)
    (movl (imm 0) eax)
    (addl (imm si) esp)
    (jmp '__arg_count_error)
    (label cont-label)))

(def emit-at-least-arg-count-check (si n)
  (with (error-label (unique-label)
	 cont-label (unique-label))
    (cmp (imm n) eax)
    ;(jl error-label)
    ;(jmp cont-label)
    (jge cont-label)
    ;(label error-label)
    (movl (imm 0) eax)
    (addl (imm si) esp)
    (jmp '__arg_count_error)
    (label cont-label)))

(def emit-unbound-check (si)
  ; expects value to check in eax and corresponding symbol in ebx
  (with (error-label (unique-label)
	 cont-label (unique-label))
    (cmp (imm unbound-val) eax)
    (jne cont-label)
    (movl (imm frame-sentinel) (deref si esp))
    (movl (imm 0) (deref (next-si si) esp))
    (movl ecx (deref (next-si-n si 2) esp))
    (movl (imm 1) eax)
    (addl (imm si) esp)
    (movl '__unbound_error_fun ecx)
    (call (unref-call (deref 2 ecx)))
    (subl (imm si) esp)
    (label cont-label)))

(def emit-bounds-check (si tag reg)
  ; emit error checking on vector/string access. Expects vector/string on the 
  ; stack and the index in eax
  ; if reg is given the vector/string is supposed to be there
  (with (error-label (unique-label)
	 cont-label (unique-label))
    (if reg 
      (movl reg ebx)
      (movl (deref si esp) ebx))
    (movl (deref (- tag) ebx) ebx) ; get size
    (cmp (imm 0) eax)
    (jl error-label) ; check for negative index
    (cmp ebx eax)
    ;(jge error-label)
    ;(jmp cont-label)
    (jl cont-label)
    (label error-label)
    (movl (imm 0) eax)
    (addl (imm si) esp)
    (jmp '__bounds_error)
    (label cont-label)))

(def fxadd1 (si env arg)
  (emit-expr si env arg)
  (emit-is-fx si env)
  (addl (imm (imm-rep 1)) eax))

(install-primop 'fxadd1 fxadd1 1 nil)

(def fxsub1 (si env arg)
  (emit-expr si env arg)
  (emit-is-fx si env)
  (subl (imm (imm-rep 1)) eax))

(install-primop 'fxsub1 fxsub1 1 nil)

(def fxlognot (si env arg)
  (emit-expr si env arg)
  (emit-is-fx si env)
  (xorl (imm (imm-rep -1)) eax))

(install-primop 'fxlognot fxlognot 1 nil)

(def fx->char (si env arg)
  (emit-expr si env arg)
  (emit-is-fx si env)
  (shll (imm (- chshift fxshift)) eax)
  (op-orl (imm chtag) eax))

(install-primop 'fx->char fx->char 1 nil)

(def char->fx (si env arg)
  (emit-expr si env arg)
  (emit-is-ch si env)
  (shrl (imm (- chshift fxshift)) eax))

(install-primop 'char->fx char->fx 1 nil)

(def fxp (si env arg)
  (emit-expr si env arg)
  (emit-type-pred fxmask fxtag))

(install-primop 'fxp fxp 1 nil)

(def fxzerop (si env arg)
  (emit-expr si env arg)
  (emit-is-fx si env)
  (cmp (imm (imm-rep 0)) eax)
  (sete al)
  (movzbl al eax)
  (sal (imm bool-bit) al)
  (op-or (imm nil-val) al))

(install-primop 'fxzerop fxzerop 1 nil)

(def nullp (si env arg)
  (emit-expr si env arg)
  (cmp (imm nil-val) eax)
  (sete al)
  (movzbl al eax)
  (sal (imm bool-bit) al)
  (op-or (imm nil-val) al))

(install-primop 'nullp nullp 1 nil)

(make-alias 'not 'nullp)

(def op-charp (si env arg)
  (emit-expr si env arg)
  (emit-type-pred chmask chtag))

(install-primop 'charp op-charp 1 nil)

(def op-consp (si env arg)
  (emit-expr si env arg)
  (emit-type-pred cellmask celltag))

(install-primop 'consp op-consp 1 nil)

(def op-car (si env arg)
  (emit-expr si env arg)
  (cmp (imm (imm-rep nil)) eax)
  (let end (unique-label)
    (je end)
    (emit-is-cell si env)
    (unref car-offset eax eax)
    (label end)))

(install-primop 'car op-car 1 nil)

(def op-cdr (si env arg)
  (emit-expr si env arg)
  (cmp (imm (imm-rep nil)) eax)
  (let end (unique-label)
    (je end)
    (emit-is-cell si env)
    (unref cdr-offset eax eax)
    (label end)))

(install-primop 'cdr op-cdr 1 nil)

(def emit-build-cons ()
  ; saves current %ebp pointer in %eax, marks it as a cons cell and bumps %ebp
  ;(movl ebp eax)
  ;(addl (imm cell-size) ebp)
  (op-orl (imm celltag) eax))

(def op-cons (si env the-car the-cdr)
  (emit-expr si env the-car)
  (movl eax (deref si esp))
  (emit-expr (next-si si) env the-cdr)
  (movl eax (deref (next-si si) esp))
  (emit-call-expand-heap (next-si-n si 2) (* 2 wordsize))
  (movl (deref (next-si si) esp) ebx)
  (movl ebx (deref wordsize eax))
  (movl (deref si esp) ebx)
  (movl ebx (deref 0 eax))
  (emit-build-cons))

(install-primop 'cons op-cons 2 nil)

(def op-setcar (si env the-cons value)
  (emit-expr si env the-cons)
  (emit-is-cell si env)
  (movl eax (deref si esp))
  (emit-expr (next-si si) env value)
  (movl (deref si esp) ebx)
  (movl eax (deref car-offset ebx)))

(install-primop 'setcar op-setcar 2 nil)

(def op-setcdr (si env the-cons value)
  (emit-expr si env the-cons)
  (emit-is-cell si env)
  (movl eax (deref si esp))
  (emit-expr (next-si si) env value)
  (movl (deref si esp) ebx)
  (movl eax (deref cdr-offset ebx)))

(install-primop 'setcdr op-setcdr 2 nil)

; a symbol has three values: a string, a value, a plist
(def op-mksymbol (si env string-expr)
  (emit-expr si env string-expr)
  (emit-is-str si env)
  (emit-save si eax)
  (emit-call-expand-heap (next-si si) (* 4 wordsize))
  ;(movl eax ebp)
  (emit-load si ebx);eax)
  ;(emit-is-str si env)
  (movl ebx (deref 0 eax))
  (movl (imm unbound-val) (deref wordsize eax))
  (movl (imm nil-val) (deref (* 2 wordsize) eax))
  ;(movl ebp eax)
  ;(addl (imm (* 4 wordsize)) ebp) ; round at 8 byte boundaries
  (op-orl (imm symboltag) eax))

(install-primop 'mksymbol op-mksymbol 1 nil)

(def op-symbolp (si env arg)
  (emit-expr si env arg)
  (emit-type-pred symbolmask symboltag))

(install-primop 'symbolp op-symbolp 1 nil)

(def op-get-symbol-value (si env sym)
  (emit-expr si env sym)
  (emit-is-sym si env)
  (movl eax ecx)
  (unref symbolval-offset eax eax)
  (emit-unbound-check si))

(install-primop 'get-symbol-value op-get-symbol-value 1 nil)

(def op-get-symbol-string (si env sym)
  (emit-expr si env sym)
  (emit-is-sym si env)
  (unref symbolstring-offset eax eax))

(install-primop 'get-symbol-string op-get-symbol-string 1 nil)

(def op-set-symbol-value (si env sym arg)
  (emit-expr si env sym)
  (emit-is-sym si env)
  (emit-save si eax)
  (emit-expr (next-si si) env arg)
  (emit-load si ebx)
  (movl eax (deref symbolval-offset ebx)))

(install-primop 'set-symbol-value op-set-symbol-value 2 nil)

(def op-get-symbol-plist (si env sym)
  (emit-expr si env sym)
  (emit-is-sym si env)
  (unref symbolplist-offset eax eax))

(install-primop 'get-symbol-plist op-get-symbol-plist 1 nil)

(def op-set-symbol-plist (si env sym arg)
  (emit-expr si env sym)
  (emit-is-sym si env)
  (emit-save si eax)
  (emit-expr (next-si si) env arg)
  (emit-load si ebx)
  (movl eax (deref symbolplist-offset ebx)))
  
(install-primop 'set-symbol-plist op-set-symbol-plist 2 nil)

(def op-vecp (si env arg)
  (emit-expr si env arg)
  (emit-type-pred vecmask vectag))

(install-primop 'vecp op-vecp 1 nil)

;; vector:
;; ---------------------------------------
;; | length   | field 0  | filed 1  | ...
;; ---------------------------------------
;; | wordsize | wordsize | wordsize | ...
;; ---------------------------------------

(def op-mkvec (si env size elem)
  (emit-expr si env size)
  (emit-is-fx si env)
  (emit-save si eax) ; save length
  ; !!! warning: stack position (next-si si) unknown during evaluation
  ; of elem (could be a problem for GC) !!!
  (emit-expr (next-si-n si 2) env elem)
  (emit-save (next-si-n si 2) eax) ; save elem
  ; round up size to be at 2*wordsize-byte boundaries
  ; and add space for length field
  (emit-load si eax)
  (addl (imm (+ wordsize (- (* 2 wordsize) 1))) eax)
  (andl (imm (- (* 2 wordsize))) eax)
  (emit-save (next-si si) eax) ; save size
  (emit-call-expand-heap (- si (* 3 wordsize)))
  (emit-load si ebx)
  (movl ebx (deref 0 eax)) ; set length field
  ;(movl ebp eax)
  (movl eax ebp)
  (emit-load (next-si si) ebx) ; get back size
  ; loop to init all elements to the value of elem
  (with (loop-label (unique-label)
	 end-label (unique-label))
    (emit-load (next-si-n si 2) ecx)
    (addl (imm wordsize) ebp)
    (subl (imm wordsize) ebx)
    (label loop-label)
    (cmp (imm 0) ebx)
    (je end-label)
    (movl ecx (deref 0 ebp))
    (addl (imm wordsize) ebp)
    (subl (imm (imm-rep 1)) ebx)
    (jmp loop-label)
    (label end-label)
    (op-orl (imm vectag) eax)))

(install-primop 'mkvec op-mkvec 2 nil)

(def op-vec-set (si env arg1 arg2 arg3)
  (emit-expr si env arg1)
  (emit-is-vec si env)
  (emit-save si eax)
  (emit-expr (next-si si) env arg2)
  (emit-is-fx (next-si si) env)
  (emit-bounds-check si vectag nil)
  (emit-save (next-si si) eax)
  (emit-expr (next-si-n si 2) env arg3)
  (emit-load si ebx) ; get vector adress
  (addl (deref (next-si si) esp) ebx) ; sum offset
  (movl eax (deref (+ wordsize (- vectag)) ebx))) ; set value

(install-primop 'vec-set op-vec-set 3 nil)

(def op-vec-ref (si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-is-vec si env)
  (emit-save si eax)
  (emit-expr (next-si si) env arg2)
  (emit-is-fx (next-si si) env)
  (emit-bounds-check si vectag nil)
  (emit-load si ebx) ; get vector adress
  (addl eax ebx) ; sum offset
  (movl (deref (+ wordsize (- vectag)) ebx) eax)) ; get value

(install-primop 'vec-ref op-vec-ref 2 nil)

(def op-vec-len (si env arg1)
  (emit-expr si env arg1)
  (emit-is-vec si env)
  (movl (deref (- vectag) eax) eax))

(install-primop 'vec-len op-vec-len 1 nil)

(def emit-extended-pred (tag . mask)
  (with (mask (car mask)
         end (unique-label)
	 false (unique-label))
    (movl eax ebx)
    (andl (imm basicmask) ebx)
    (cmp (imm extendedtag) ebx)
    (jne false)
    (movl (deref (- extendedtag) eax) eax)
    (if mask
      (andl (imm mask) eax))
    (cmp (imm tag) eax)
    (jne false)
    (movl (imm (imm-rep t)) eax)
    (jmp end)
    (label false)
    (movl (imm (imm-rep nil)) eax)
    (label end)))

(def op-strp (si env arg)
  (emit-expr si env arg)
  (emit-extended-pred strtag fxmask))

(install-primop 'strp op-strp 1 nil)

; allocation initializes args to 0
(install-primop 'mkstr
  (fn (si env size-expr)
    (emit-expr si env size-expr)
    (emit-is-fx si env)
    (emit-save si eax) ; save length
    ; round up size to be at 2*wordsize-byte boundaries
    (movl eax ebx)
    ; strings are made of 1-byte cells
    (shrl (imm fxshift) ebx)
    (addl (imm (+ wordsize (- (* 2 wordsize) 1))) ebx)
    (andl (imm (- (* 2 wordsize))) ebx)
    ;(emit-save (next-si si) ebx) ; save size
    (movl ebx eax)
    (emit-call-expand-heap (next-si-n si 1))
    (emit-load si ebx)
    (movl ebx (deref 0 eax)) ; put length in its field
    ;(movl ebp eax)
    ;(emit-load (next-si si) ebx)
    ;(addl ebx ebp)
    (op-orl (imm extendedtag) eax)) 
  1 nil)

(install-primop 'str-set 
  (fn (si env arg1 arg2 arg3)
    (emit-expr si env arg1)
    (emit-is-str si env)
    (emit-save si eax)
    (emit-expr (next-si si) env arg2)
    (emit-is-fx (next-si si) env)
    (emit-bounds-check si extendedtag nil)
    (emit-save (next-si si) eax)
    (emit-expr (next-si-n si 2) env arg3)
    (emit-is-ch (next-si-n si 2) env)
    (shrl (imm chshift) eax) ; remove char tag
    (emit-load si ebx) ; get string adress
    (shrl (imm fxshift) (deref (next-si si) esp)) ; 1-byte
    (addl (deref (next-si si) esp) ebx) ; sum offset
    (movb al (deref (+ wordsize (- extendedtag)) ebx)) ; set value
    (shll (imm chshift) eax) ; return orginal char value
    (op-orl (imm chtag) eax))
  3 nil)

(install-primop 'str-ref 
  (fn (si env arg1 arg2)
    (emit-expr si env arg1)
    (emit-is-str si env)
    (emit-save si eax)
    (emit-expr (next-si si) env arg2)
    (emit-is-fx (next-si si) env)
    (emit-bounds-check si extendedtag nil)
    (emit-load si ebx) ; get string adress
    (shrl (imm fxshift) eax) ; 1-byte
    (addl eax ebx) ; sum offset
    (movb (deref (+ wordsize (- extendedtag)) ebx) al) ; get value
    (movzbl al eax)
    (shll (imm chshift) eax)
    (op-orl (imm chtag) eax))
  2 nil)

(install-primop 'str-len 
  (fn (si env arg1)
    (emit-expr si env arg1)
    (emit-is-str si env)
    (movl (deref (- extendedtag) eax) eax))
  1 nil)

(def op-vector-aux (si env i items)
  (if items
    (do
      (emit-expr (next-si si) env (car items))
      (emit-load si ebx)
      (movl eax (deref (+ wordsize (- (* wordsize i) vectag)) ebx))
      (op-vector-aux si env (+ i 1) (cdr items)))))

(install-primop 'vector 
  (fn (si env . items)
    ((emitter 'mkvec) si env (len items) nil)
    (emit-save si eax)
    (op-vector-aux si env 0 items)
    (emit-load si eax))
  0 t)

(def op-string-aux (si env i chars)
  (if chars
    (do
      ;(emit-expr (next-si si) env (car chars))
      ;(emit-is-ch (next-si si) env)
      ;(emit-load si ebx)
      ;(shrl (imm chshift) eax) ; remove char tag
      ;(mov al (deref (+ wordsize (- i extendedtag)) ebx))
      (movb (imm (char->fx (car chars))) (deref (+ wordsize (- i extendedtag)) eax))
      (op-string-aux si env (+ i 1) (cdr chars)))))

(install-primop 'string 
  (fn (si env . chars)
    (if (some [not (charp _)] chars)
      (err (make-string "Error: string accepts only immediate characters: " chars)))
    ((emitter 'mkstr) si env (len chars))
    ;(emit-save si eax)
    (op-string-aux si env 0 chars))
    ;(emit-load si eax))
  0 t)

(install-primop '__float-init 
  (fn (si env in-lbl)
;    (emit-call-expand-heap si (* 4 wordsize))
    (fldl-plain in-lbl)
    (emit-build-float si))
  1 nil)

(install-primop 'flp 
  (fn (si env expr)
    (emit-expr si env expr)
    (emit-extended-pred floattag))
  1 nil)

(def emit-build-float (si)
  ; takes double float from float register stack
  (emit-call-expand-heap si (* 4 wordsize))
  (movl (imm floattag) (deref 0 eax))
  (fstpl (deref wordsize eax))
  ;(movl ebp eax)
  (op-orl (imm extendedtag) eax))
  ;(addl (imm (* 4 wordsize)) ebp))

(def emit-fl-cmp (si env arg1 arg2 setter)
  (emit-expr si env arg1)
  (emit-is-float si env)
  (emit-save si eax)
  (emit-expr (next-si si) env arg2)
  (emit-is-float (next-si si) env)
  (emit-load si ebx)
  (fldl eax)
  (fldl ebx)
  (fucompp)
  (fnstsw ax)
  (sahf)
  (setter al)
  (movzbl al eax)
  (shll (imm bool-bit) eax)
  (op-orl (imm nil-val) eax))

(install-primop 'fl= 
  (fn (si env arg1 arg2)
    (emit-fl-cmp si env arg1 arg2 sete))
  2 nil)

(install-primop 'fl< 
  (fn (si env arg1 arg2)
    (emit-fl-cmp si env arg1 arg2 setb))
  2 nil)

(install-primop 'fl> 
  (fn (si env arg1 arg2)
    (emit-fl-cmp si env arg1 arg2 seta))
  2 nil)

(install-primop 'fl<= 
  (fn (si env arg1 arg2)
    (emit-fl-cmp si env arg1 arg2 setbe))
  2 nil)

(install-primop 'fl>= 
  (fn (si env arg1 arg2)
    (emit-fl-cmp si env arg1 arg2 setae))
  2 nil)

(def emit-fl-op-2 (si env arg1 arg2 op)
  ; emit arithmetic operation between arg1 and arg2. arg1, arg2 and result 
  ; are floating point expressions
  (emit-expr si env arg1)
  (emit-is-float si env)
  (emit-save si eax)
  (emit-expr (next-si si) env arg2)
  (emit-is-float (next-si si) env)
  ;(emit-call-expand-heap (next-si-n si 2) (* 4 wordsize)) 
  (emit-load si ebx)
  (fldl ebx)
  (op eax)
  (emit-build-float si))

(install-primop 'fl+ 
  (fn (si env arg1 arg2)
    (emit-fl-op-2 si env arg1 arg2 faddl))
  2 nil)

(install-primop 'fl- 
  (fn (si env arg1 arg2)
    (emit-fl-op-2 si env arg1 arg2 fsubl))
  2 nil)

(install-primop 'fl* 
  (fn (si env arg1 arg2)
    (emit-fl-op-2 si env arg1 arg2 fmull))
  2 nil)

(install-primop 'fl/ 
  (fn (si env arg1 arg2)
    (emit-fl-op-2 si env arg1 arg2 fdivl))
  2 nil)

(install-primop 'round 
  (fn (si env arg1)
    (emit-expr si env arg1)
    (emit-is-float si env)
    (fldl eax)
    (fistpl si)
    (emit-load si eax)
    (sall (imm fxshift) eax))
  1 nil)

(install-primop 'fx->fl 
  (fn (si env arg1)
    (emit-expr si env arg1)
    (sarl (imm fxshift) eax)
    (emit-save si eax)
   ; (emit-call-expand-heap (next-si si) (* 4 wordsize))
    (fildl si)
    (emit-build-float si))
  1 nil)

(def op-fx-2 (si env arg1 arg2 op order)
  (emit-expr si env arg1)
  (emit-is-fx si env)
  (emit-save si eax)
  (emit-expr (next-si si) env arg2)
  (emit-is-fx (next-si si) env)
  (if (is order 'rev)
    (do
      (op eax (deref si esp))
      (emit-load si eax))
    (op (deref si esp) eax)))

(install-primop 'fx+ 
  (fn (si env arg1 arg2)
    (op-fx-2 si env arg1 arg2 addl nil))
  2 nil)

(install-primop 'fx- 
  (fn (si env arg1 arg2)
    (op-fx-2 si env arg1 arg2 subl 'rev))
  2 nil)

(install-primop 'fx* 
  (fn (si env arg1 arg2)
    (emit-expr si env arg1)
    (emit-is-fx si env)
    (emit-save si eax)
    (emit-expr (next-si si) env arg2)
    (emit-is-fx (next-si si) env)
    (sarl (imm fxshift) eax)
    (imull (deref si esp) eax))
  2 nil)

(install-primop 'fx/ 
  (fn (si env arg1 arg2)
    (emit-expr si env arg1)
    (emit-is-fx si env)
    (emit-save si eax)
    (emit-expr (next-si si) env arg2)
    (emit-is-fx (next-si si) env) ; TODO: add zero check
    (movl eax ebx)
    (emit-load si eax)
    (cdq) ; sign extends dividend, idiv divides EDX:EAX
    (idivl ebx)
    ; need to shift: 4x/4y = x/y, so x/y must be shifted
    (sall (imm fxshift) eax))
  2 nil)

(install-primop 'fxrem 
  (fn (si env arg1 arg2)
    (emit-expr si env arg1)
    (emit-is-fx si env)
    (emit-save si eax)
    (emit-expr (next-si si) env arg2)
    (emit-is-fx (next-si si) env) ; TODO: add zero check
    (movl eax ebx)
    (emit-load si eax)
    (cdq) ; sign extends dividend, idiv divides EDX:EAX
    (idivl ebx)
    ; no need to shift result: rem(4x/4y) = 4rem(x/y)
    (movl edx eax)) ; gets remainder
  2 nil)

(install-primop 'fxlogand 
  (fn (si env arg1 arg2)
    (op-fx-2 si env arg1 arg2 andl nil))
  2 nil)

(install-primop 'fxlogor 
  (fn (si env arg1 arg2)
    (op-fx-2 si env arg1 arg2 op-orl nil))
  2 nil)

(def emit-fx-cmp (si env arg1 arg2 setter)
  ; generic comparision emitter
  (emit-expr si env arg1)
  (emit-save si eax)
  (emit-expr (next-si si) env arg2)
  (cmp (deref si esp) eax) ; arg2 - arg1
  (setter al)
  (movzbl al eax)
  (shll (imm bool-bit) eax)
  (op-orl (imm nil-val) eax))

(install-primop 'is 
  (fn (si env arg1 arg2) 
    (emit-fx-cmp si env arg1 arg2 sete))
  2 nil)

(make-alias 'fx= 'is)

(install-primop 'fx< 
  (fn (si env arg1 arg2)
    (emit-fx-cmp si env arg1 arg2 setg))
  2 nil)

(install-primop 'fx<= 
  (fn (si env arg1 arg2)
    (emit-fx-cmp si env arg1 arg2 setge))
  2 nil)

(install-primop 'fx> 
  (fn (si env arg1 arg2)
    (emit-fx-cmp si env arg1 arg2 setl))
  2 nil)

(install-primop 'fx>= 
  (fn (si env arg1 arg2)
    (emit-fx-cmp si env arg1 arg2 setle))
  2 nil)

; characters can be compared as fixnums
(make-alias 'ch= 'fx=)
(make-alias 'ch> 'fx>)
(make-alias 'ch< 'fx<)
(make-alias 'ch>= 'fx>=)
(make-alias 'ch<= 'fx<=)

(install-primop 'fnp 
  (fn (si env arg)
    (emit-expr si env arg)
    (emit-type-pred basicmask closuretag))
  1 nil)

(install-primop '__print_backtrace 
  (fn (si env)
    (lea (deref si esp) eax) 
    (emit-save (next-si si) eax)
    (emit-save si edi)
    (addl (imm (next-si si)) esp)
    (call "print_backtrace")
    (subl (imm (next-si si)) esp)
    (movl (imm (imm-rep nil)) eax))
  0 nil)

(install-primop '__load 
  (fn (si env filename)
    (emit-expr si env filename)
    (emit-save si eax)
    (addl (imm si) esp)
    (call "full_load")
    (subl (imm si) esp)
    (cmp (imm 0) eax)
    (let ok-label (unique-label)
      (jne ok-label)
      (movl (imm 0) eax)
      (addl (imm si) esp)
      (jmp '__load_error)
      (label ok-label)
      (movl (imm frame-sentinel) (deref si esp))
      (addl (imm si) esp)
      (call (unref-call eax))
      (subl (imm si) esp)))
  1 nil)

(set count* 0)

(def unique-label ()
  (let l (str-append "L_" (to-string count*))
    (set count* (+ count* 1))
    l))

;; if form

(def ifp (expr)
  (if (is (car expr) if-sym)
    (do
      (if (or (< (len expr) 3) (> (len expr) 4))
        (err "Wrong number of arguments to if"))
      t)))

(def if-test (x)
  (cadr x))

(def if-conseq (x)
  (caddr x))

(def if-altern (x)
  (cadddr x))

(def emit-if (si env expr tail)
  (with (altern (unique-label)
	 end-label (unique-label))
    (emit-expr si env (if-test expr))
    (cmp (imm (imm-rep nil)) eax)
    (je altern)
    (if tail
      (emit-tail-expr si env (if-conseq expr))
      (emit-expr si env (if-conseq expr)))
    (jmp end-label)
    (label altern)
    (if tail
      (emit-tail-expr si env (if-altern expr))
      (emit-expr si env (if-altern expr)))
    (label end-label)))

;; let form

(def letp (expr)
  (if (and (consp expr) (is (car expr) let-sym))
    (do
      (if (not (is (len expr) 3))
        (err "Wrong number of arguments to let"))
      (if (some (compose not symbolp binding-name) (let-bindings expr))
        (err "Malformed let expression"))
      t)))

(def normalize-let-bnds (bnds)
  (map (fn (x) (if (atom x) (list x) x)) bnds))

(def let-bindings (expr)
  (normalize-let-bnds (cadr expr)))

(def let-body (expr)
  (caddr expr))

(def binding-name (b)
  (if (consp b)
      (car b)
      b))

(def binding-expr (b)
  (if (consp b)
      (cadr b)
      nil))

(def let-names (e)
  (map binding-name (let-bindings e)))

(def let-exprs (e)
  (map binding-expr (let-bindings e)))

(def emit-let-bnds (si env new-env bnds)
  (if bnds
    (with (name (binding-name (car bnds)) 
           val-expr (binding-expr (car bnds)))
      (emit-expr si env val-expr)
      (emit-save si eax)
      (emit-let-bnds (next-si si) env (bind-var new-env name si) (cdr bnds)))
    (list si new-env)))

(def emit-let (si env expr tail)
  (let res (emit-let-bnds si env env (let-bindings expr))
    (with (si (car res) env (cadr res))
      (if tail
	  (emit-tail-expr si env (let-body expr))
	  (emit-expr si env (let-body expr))))))

(def varp (expr)
  (and (not (is expr t)) (not (is expr nil)) (symbolp expr)))

; local values have a negative stack index
; closed vars have a positive closure index
; global consts are strings

(def localp (i)
  (and (fxp i) (<= i 0)))

(def closedp (i)
  (and (fxp i) (> i 0)))

(def constp (i)
  (strp i))

(def emit-var-ref (env expr)
  (let i (lookup env expr)
    (if (not i)
      (err (make-string "Variable not found: " expr)))
    (if (localp i) (emit-load i eax)
	(closedp i) (movl (deref (- i closuretag) edi) eax)
        (constp i) (movl i eax)
	(err "Unkown variable type"))))

; setq form

(def setq-p (expr)
  (if (and (consp expr) (is (car expr) setq-sym))
    (do
      (if (not (is (len expr) 3))
        (err "Wrong number of arguments to setq"))
      t)))

(def setq-var (expr)
  (cadr expr))

(def setq-val (expr)
  (caddr expr))

(def emit-setq (si env expr)
  (emit-expr si env (setq-val expr))
  (emit-set-from-previous si env (setq-var expr)))

(def emit-set-from-previous (si env var-name)
  ; set a variable with the value currently in eax
  (let i (lookup env var-name)
    (if (not i)
      (err (make-string "Variable not found: " var-name)))
    (if (localp i) (emit-save i eax)
        (constp i) (movl eax i)
        (closedp i) (err "Cannot set closed var, use a cons instead")
        (err "Unkown variable type"))))

(def mk-empty-env () 
  (list (cons 'intern (to-string 'intern))
        (cons '__error_continuation (to-string '__error_continuation))
        (cons 't (imm (imm-rep t)))
        (cons 'nil (imm (imm-rep nil)))))

(def lookup (env name)
  ; lookups a variable name in an environment
  (let res (assoc name env)
    (cdr res)))

(def bind-var (env name value)
  (cons (cons name value) env))

; labels form

(def labelsp (expr)
  (and (consp expr) (is (car expr) 'labels)))

(def labels-bindings (expr)
  (cadr expr))

(def labels-body (expr)
  (caddr expr))

(def nlabels (n)
  (if (is n 0) nil (cons (unique-label) (nlabels (- n 1)))))

(def emit-labels-extend-env (env labls bnds)
  (if bnds
    (emit-labels-extend-env 
      (bind-var env (binding-name (car bnds)) (car labls))
      (cdr labls) (cdr bnds))
    env))

; returns global environment
(def emit-labels (expr env minimal-p)
  (with (labls (map (compose to-string binding-name) (labels-bindings expr));(nlabels (length (labels-bindings expr))))
	 bodies (map binding-expr (labels-bindings expr)))
    (withs (env (emit-labels-extend-env env labls (labels-bindings expr))
            init-const-args (map
                              (fn (lbl expr)
	                        (if (codep expr) 
                                      (emit-code env lbl expr minimal-p)
                                    ; check for (const (float ...)) must come 
                                    ; before check for (const ...) or it will 
                                    ; never be reached
	                            (const-float-type-p expr) 
                                      (emit-const-float lbl expr)
	                            (const-type-p expr) 
                                      (emit-const lbl minimal-p
                                                  "long" wordsize nil-val)
	                            (err "Invalid expression in labels form")))
	                      labls bodies)
            body (gen-const-init-code labls bodies init-const-args
                                      (labels-body expr)))
      (emit-init-entry body env))))

; const form 

(def map-if (f l . args)
  (if l
    (let res (apply f (car l) (map (fn (x) (car x)) args))
      (if res
        (cons res (apply map-if f (cdr l) (map (fn (x) (cdr x)) args)))
        (apply map-if f (cdr l) (map (fn (x) (cdr x)) args))))))

(def gen-const-init-code (lbls exprs args body)
  ; generate lisp expression needed to initialize constants
  (list 'do
	(cons 'do 
	      (map-if 
	        (fn (l e arg) 
		  (if (const-float-type-p e) 
                        (list '%%const-init l (list '__float-init arg))
                      (const-type-p e) 
                        (list '%%const-init l (const-expr e))))
	        lbls exprs args))
	body))

(def const-type-p (expr)
  (and (consp expr) (is (car expr) 'const)))

(def const-float-type-p (expr)
  (and (consp expr) (is (car expr) 'const) 
       (consp (cadr expr)) (is (caadr expr) 'float)))

(def const-expr (expr)
  (cadr expr))

(def emit-const (lbl minimal-p type size val)
  (if minimal-p
    (globl lbl))
  (emit "    .data")
  (emit "    .type " lbl ", @object")
  (emit "    .size " lbl ", " size)
  (label lbl)
  (emit "    ." type " " val)
  nil)

(def emit-const-float (lbl expr)
  (let lbl2 (unique-label)
    (label lbl2)
    (emit "    .double 0f" (cadr (cadr expr)))
    (emit-const lbl nil "long" wordsize nil-val)
    lbl2))

(def emit-save-in-const-roots (lbl)
  ; save const adress in __const_roots to aid the GC
  (movl "__const_roots" ebx)
  (movl "__const_roots_top" ecx)
  (shll (imm 2) ecx)
  (addl ecx ebx)
  (shrl (imm 2) ecx)
  (movl (imm lbl) (deref 0 ebx))
  (addl (imm 1) ecx)
  (movl ecx "__const_roots_top"))

(install-primop '%%const-init 
  (fn (si env arg1 arg2)
    (emit-expr si env arg2)
    (movl eax arg1)
    (emit-save-in-const-roots arg1))
  2 nil)
  
; code form

(def codep (expr)
  (and (consp expr) (is (car expr) 'code)))

(def code-args (expr)
  (if (code-args-has-rest expr)
      (if (symbolp (cadr expr))
	  (list (code-args-rest-arg expr))
	  (append
	   (append (butlast (cadr expr)) (list (car (last (cadr expr)))))
	   (list (code-args-rest-arg expr))))
      (cadr expr)))

(def code-args-has-rest (expr)
  (or (and (cadr expr) (symbolp (cadr expr)))
      (not (is (cdr (last (cadr expr))) nil))))

(def code-args-rest-arg (expr)
  (if (and (cadr expr) (symbolp (cadr expr)))
      (cadr expr)
      (cdr (last (cadr expr)))))

(def code-free-vars (expr)
  (caddr expr))

(def code-body (expr)
  (cadddr expr))

(def emit-build-rest-arg (rest-si n)
  ; emits code for building rest arg list, takes stack location of rest arg 
  ; (and of the first arg to be collected) and the number of non-rest args
  (subl (imm n) eax)
  (shll (imm 2) eax)
  (subl eax esp)
  (emit-save rest-si eax)
  (shll (imm 1) eax)
  (emit-call-expand-heap (next-si rest-si))
  (movl eax ebp)
  (emit-load rest-si eax)
  (movl esp edx)
  (addl eax esp)
  (addl (imm (- (* n wordsize))) edx)
  (with (loop-start (unique-label)
	 loop-end (unique-label)
	 non-rest-si (+ rest-si wordsize))
    ; %edx: stack adress of current rest arg considered
    ; %ecx: stack adress of last non-rest arg
    ; %ebx: list built so far
    (lea (deref non-rest-si esp) ecx)
    (movl (imm (imm-rep nil)) ebx) ; start with empty list
    (label loop-start)
    (cmp ecx edx) ; args finished?
    (je loop-end)
    ; build the list
    (movl ebx (deref wordsize ebp)) ; saves %ebx in cdr part
    (movl (deref 0 edx) ebx)
    (movl ebx (deref 0 ebp)) ; saves current stack pos in car part
    ;(emit-build-cons)
    (movl ebp ebx) ; saves current cons in ebx
    (op-orl (imm celltag) ebx) ; tag it
    (addl (imm (* 2 wordsize)) ebp) ; go to next cell
    ;(emit "    addl $~a, %ebp" cell-size)
    ;(movl eax ebx) ; saves current cons
    (addl (imm wordsize) edx) ; next index
    (jmp loop-start)
    (label loop-end)
    (emit-save rest-si ebx))) ; saves list in rest arg pos

(def emit-code-a (n-args body rest-arg si ci env args free-vars)
  (if args (emit-code-a n-args body rest-arg (next-si si) ci 
                        (bind-var env (car args) si) (cdr args) free-vars)
      free-vars (emit-code-a n-args body rest-arg si (+ ci wordsize) 
                             (bind-var env (car free-vars) ci)
			     args (cdr free-vars))
      (do 
        (if rest-arg 
	  (emit-build-rest-arg (+ si wordsize) (- n-args 1)))
        (emit-tail-expr si env body))))

(def emit-code (env label expr minimal-p)
  (if minimal-p
    (globl label))
  (emit-fun-header label)
  (withs (args (code-args expr)
	  rest-arg (code-args-rest-arg expr)
	  n-args (len args)
	  free-vars (code-free-vars expr)
	  ci (* 3 wordsize) ; closure index, leave space for adress and length
	  body (code-body expr))
    (if rest-arg
	(emit-at-least-arg-count-check (- wordsize) (- n-args 1))
	(emit-exact-arg-count-check (- wordsize) env n-args))
    (emit-code-a n-args body rest-arg (- wordsize) ci env args free-vars)))

; labelcall form

(def labelcall-p (expr)
  (and (consp expr) (is (car expr) 'labelcall)))

(def labelcall-name (expr)
  (cadr expr))

(def labelcall-args (expr)
  (cddr expr))

(def emit-move-args (first-i last-i base-i)
  (if (>= first-i last-i)
    (do
      (emit-load first-i eax)
      (emit-save base-i eax)
      (emit-move-args (- first-i wordsize) last-i (- base-i wordsize)))))

;; stack after label call (non tail)
;;
;; | .............  |
;; ------------------
;; | frame-sentinel |
;; ------------------ %esp
;; | return address |
;; ------------------ 
;; |   arg 1        |
;; ------------------
;; | .............. |

(def emit-labelcall-a (si env args)
  (if args
    (do
      (emit-expr si env (car args))
      (emit-save si eax)
      (emit-labelcall-a (next-si si) env (cdr args)))))

(def emit-labelcall (si env expr tail)
  ; clear space reserved for frame-sentinel and ret. address
  (emit-save si (imm 0))
  (emit-save (next-si si) (imm 0))
  (emit-labelcall-a (next-si-n si 2) env (labelcall-args expr))
  (if tail
      (do
	(emit-move-args (next-si-n si 2)
			(- si (+ wordsize
				 (* wordsize (len (labelcall-args expr)))))
			(- wordsize))
	; for arg. count check
	(movl (imm (len (labelcall-args expr))) eax)
	(jmp (lookup env (labelcall-name expr))))
      (do
	; adjust %esp, si is negative
	(emit-save si (imm frame-sentinel))
	(addl (imm si) esp)
	; for arg. count check
	(movl (imm (len (labelcall-args expr))) eax) 
	(call (lookup env (labelcall-name expr)))
	(subl (imm si) esp)))) ; adjust %esp

;; closure form: creates a closure
;; (closure label-name closure-name val1 val2 ...)
;; a closure is an array of:
;; Position:  0          1                    2              3          ...
;;          ---------------------------------------------------------------
;; Content: | number of vars + 1 |address   | closure name | var1     | ...
;;          ---------------------------------------------------------------
;; Size:    | wordsize           | wordsize | wordsize     | wordsize | ...
;;          ---------------------------------------------------------------

(def closurep (expr)
  (and (consp expr) (is (car expr) 'closure)))

(def closure-label (expr)
  (cadr expr))

(def closure-name (expr)
  (caddr expr))

(def closure-free-vars-values (expr)
  (cdddr expr))

(def round-at-boundary (n boundary)
  (* (/ (+ n (- boundary 1)) boundary) boundary))

(def emit-closure-vals (si env ci vals)
  (if vals
    (do
      (emit-expr si env (car vals))
      ; !!! WARNING: vals expressions must not touch the ebp register   !!!
      ; !!! compiler generated code respect this, but not user code     !!!
      ; !!! or this function has to change or user shouldn't be able to !!!
      ; !!! use the (closure ...) form                                  !!!
      (movl eax (deref ci ebp))
      (emit-closure-vals si env (+ ci wordsize) (cdr vals)))))

(def emit-closure (si env expr)
  (withs (label (closure-label expr)
	  name (closure-name expr)
	  vals (closure-free-vars-values expr)
	  size (* wordsize (+ 3 (len vals))))
    (emit-expr si env name)
    (emit-save si eax)
    ;(emit "    movl %eax, ~a(%esp)" si)
    (emit-call-expand-heap (next-si si) 
                           (round-at-boundary size (* 2 wordsize)))
    (movl eax ebp)
    (movl (imm (lookup env label)) 
          (deref (+ closuretag closureaddr-offset) ebp)) ; address
    (movl (imm (imm-rep (+ (len vals) 1))) 
          (deref (+ closuretag closurelen-offset) ebp)) ; length
    (emit-load si eax)
    (movl eax (deref (* 2 wordsize) ebp)) ; save closure name
    (emit-closure-vals si env (* 3 wordsize) vals)
    (movl ebp eax)
    (op-orl (imm closuretag) eax)))
    ; increase heap index and round at 8-byte boundaries
    ;(addl (imm (+ size (- (* 2 wordsize) 1))) ebp)
    ;(andl (imm (- (* 2 wordsize))) ebp)))

(def emit-unrolled-arg (si env)
  ; emit code to unroll current stack location (wich must be a list) into the 
  ; stack. %eax will hold the number of args unrolled
  (with (loop-start (unique-label)
	 loop-end (unique-label))
    ; %eax: list yet to be unrolled
    ; %ecx: number of args unrolled
    ; %edx: current stack adress where to unroll the list
    (emit-load si eax) ; gets list to unroll
    (movl (imm 0) ecx)
    (lea (deref si esp) edx)
    (label loop-start)
    (cmp (imm (imm-rep nil)) eax) ; end of list?
    (je loop-end)
    (movl esp ebp)
    (movl edx esp)
    (emit-is-cell (- wordsize) env) ; must be a cons
    (movl ebp esp)
    (movl (deref car-offset eax) ebx) ; takes the car
    (movl ebx (deref 0 edx)) ; puts it into the stack
    (subl (imm wordsize) edx) ; next stack location
    (addl (imm 1) ecx) ; one more arg unrolled
    (movl (deref cdr-offset eax) eax) ; proceed with cdr
    (jmp loop-start)
    (label loop-end)
    (movl ecx eax)))

; funcall form: calls a closure
(def funcallp (expr)
  (and (consp expr) (is (car expr) 'funcall)))

(def funcall-closure (expr)
  (cadr expr))

(def funcall-args (expr)
  (cddr expr))

; stack after funcall (non tail)

; | ...................... |
; -------------------------- si(%esp) before call
; | caller closure pointer |
; --------------------------
; | frame sentinel         |
; -------------------------- %esp
; | caller return address  |
; --------------------------
; |   arg 1                |
; --------------------------
; | ...................... |

; return stack index of the nth argument of the current function
; count starts from 0
(def fn-arg-si (n)
  (- (* (+ n 1) wordsize)))

(def emit-funcall-a (si env args)
  (if args
    (do
      (if (immediatep (car args)) ; optimization if arg is an immediate
        (emit-save si (imm (imm-rep (car args))))
        (do
          (emit-expr si env (car args))
          (emit-save si eax)))
      (emit-funcall-a (next-si si) env (cdr args)))))

(def emit-funcall (si env expr tail apply-p)
  ; emits code for a function call, if tail is t treats function call as a 
  ; tail call, if apply-p is t unrolls the last argument into the stack
  (emit-expr si env (funcall-closure expr))
  (emit-save si eax) ; save closure
  (emit-funcall-real si env (fn (si env)
                              (emit-funcall-a si env (funcall-args expr)))
                     (len (funcall-args expr)) tail apply-p))

(def emit-call-table-lookup ()
  (with (go-on (unique-label)
         error (unique-label))
    (movl edi ebx)
    (op-and (imm basicmask) ebx) ; set to basic tag
    (shll (imm 2) ebx) ; hardwired: 2 = log2(wordsize), wordsize=4
    (addl (imm call-table-lbl) ebx) ; ebx is the offset in the table
    (jmp (unref-call (deref 0 ebx)))))

(def emit-funcall-real (si env args-generator n-args tail apply-p)
  ; emit code for calling function in si(%esp) and use args-generator
  ; to emit code to pass the arguments
  ; clear space reserved for frame-sentinel and ret. adress
  (emit-save (next-si si) (imm frame-sentinel));0))
  (emit-save (next-si-n si 2) (imm 0))
  ; leave space for previous closure pointer, 
  ; for frame-sentinel and for return adress
  ;(emit-funcall-a (next-si-n si 3) env (funcall-args expr))
  (args-generator (next-si-n si 3) env)
  (let last-si (- si (+ (* 2 wordsize) 
			(* wordsize n-args)));(len (funcall-args expr)))))
    (if tail
	(do
	  (emit-load si edi)
	  (emit-move-args (next-si-n si 3) last-si (- wordsize))
	  (if apply-p
	    (do
              (emit-unrolled-arg 
                (- (* wordsize n-args)) env)
	      (addl (imm (- n-args 1)) eax))
            (movl (imm n-args) eax))
          (emit-call-table-lookup))
	(do
	  ; set callee closure pointer and save caller closure pointer
	  (movl edi ebx)
	  (emit-load si edi)
	  (emit-save si ebx)
	  ; put frame sentinel
	  ;(emit-save (next-si si) (imm frame-sentinel))
	  (if apply-p
            (do
              (emit-unrolled-arg last-si env)
	      (addl (imm (- n-args 1)) eax))
            (movl (imm n-args) eax))
	  ; adjust %esp, si is negative
	  (addl (imm (next-si-n si 2)) esp)
          (let ret-point (unique-label)
            ;(pushl (imm ret-point)) ; push return address
            (emit-save 0 (imm ret-point))
	    (emit-call-table-lookup)
            (label ret-point))
	  (subl (imm (next-si si)) esp) ; adjust %esp
	  (emit-load si edi))))) ; restore cl. pointer

; apply form

(def apply-p (expr)
  (and (consp expr) (is (car expr) 'apply)))

(def emit-apply (si env expr)
  (emit-funcall si env expr nil t))

(def emit-tail-apply (si env expr)
  (emit-funcall si env expr t t))

; __ccc form
; (__ccc one-arg-function)

(def ccc-p (expr)
  (and (consp expr) (is (car expr) '__ccc)
       (if (is (len expr) 2) 
         t
         (err (make-string "Wrong number of arguments to " expr)))))

(def ccc-fn (expr)
  (cadr expr))

(def emit-ccc (si env expr tail)
  (let ret-label (unique-label)
    ; build the continuation
    (emit-call-stack-copy-rev si)
    (movl (imm continuation-tag) (deref 0 eax))
    (movl main-stack-base ebx)
    (subl esp ebx) ; calculate stack len
    (subl (imm si) ebx)
    (movl ebx (deref wordsize eax))
    (movl (imm ret-label) (deref (* 2 wordsize) eax)) ; ret. adress
    (movl esp (deref (* 3 wordsize) eax))
    (movl edi (deref (* 4 wordsize) eax))
    (op-orl (imm extendedtag) eax)
    (emit-save si eax) ; save continuation
    (emit-expr (next-si si) env (ccc-fn expr))
    (emit-is-closure (next-si si) env)
    (emit-load si ebx)
    (emit-save si eax) ; put function in si(%esp)
    (emit-funcall-real si env (fn (si env) (emit-save si ebx)) 1 tail nil)
    (label ret-label)
    (if tail
      (emit-fun-ret))))

; do form
; sequentially executes instructions

(def do-p (expr)
  (and (consp expr) (is (car expr) 'do)))

(def do-forms (expr)
  (cdr expr))

(def emit-do (si env expr tail)
  (mapl1
    (fn (forms)
      (if (or (cdr forms) (not tail))
        (emit-expr si env (car forms))
	(emit-tail-expr si env (car forms))))
    (do-forms expr)))

; FFI

(def ffi-p (expr)
  (and (consp expr) (is (car expr) 'ffi-call)))

(def ffi-name (expr)
  (cadr expr))

(def ffi-args (expr)
  (cddr expr))

(def emit-ffi-call-a (si env args expr)
  (if args
    (do
      (emit-expr si env (car args))
      (emit-save si eax)
      (emit-ffi-call-a (next-si si) env (cdr args) expr))
    (do
      (addl (imm (+ si wordsize)) esp)
      (call (ffi-name expr))
      (subl (imm (+ si wordsize)) esp))))

(def emit-ffi-call (si env expr tail)
  (emit-ffi-call-a si env (rev (ffi-args expr)) expr)
  (if tail
    (emit-fun-ret)))

(def emit-tail-immediate (expr)
  (emit-immediate expr)
  (emit-fun-ret))

(def emit-tail-var-ref (env expr)
  (emit-var-ref env expr)
  (emit-fun-ret))

(def emit-tail-setq (si env expr)
  (emit-setq si env expr)
  (emit-fun-ret))

(def emit-tail-let (si env expr)
  (emit-let si env expr t))

(def emit-tail-if (si env expr)
  (emit-if si env expr t))

(def emit-tail-primcall (si env expr)
  (emit-primcall si env expr)
  (emit-fun-ret))

(def emit-tail-labelcall (si env expr)
  (emit-labelcall si env expr t))

(def emit-tail-closure (si env expr)
  (emit-closure si env expr)
  (emit-fun-ret))

(def emit-tail-funcall (si env expr)
  (emit-funcall si env expr t nil))

(def emit-tail-do (si env expr)
  (emit-do si env expr t))

(def emit-tail-expr (si env expr)
  (if 
    (immediatep expr) (emit-tail-immediate expr)
    (varp expr) (emit-tail-var-ref env expr)
    (setq-p expr) (emit-tail-setq si env expr)
    (letp expr) (emit-tail-let si env expr)
    (ifp expr) (emit-tail-if si env expr)
    (primcallp expr) (emit-tail-primcall si env expr)
    (closurep expr) (emit-tail-closure si env expr)
    (labelcall-p expr) (emit-tail-labelcall si env expr)
    (funcallp expr) (emit-tail-funcall si env expr)
    (ccc-p expr) (emit-ccc si env expr t)
    (apply-p expr) (emit-tail-apply si env expr)
    (do-p expr) (emit-tail-do si env expr)
    (ffi-p expr) (emit-ffi-call si env expr t)
    (err (make-string "Unknown expression: " expr))))

(def emit-expr (si env expr)
  (if 
    (immediatep expr) (emit-immediate expr)
    (varp expr) (emit-var-ref env expr)
    (setq-p expr) (emit-setq si env expr)
    (letp expr) (emit-let si env expr nil)
    (ifp expr) (emit-if si env expr nil)
    (primcallp expr) (emit-primcall si env expr)
    (closurep expr) (emit-closure si env expr)
    (labelcall-p expr) (emit-labelcall si env expr nil)
    (funcallp expr) (emit-funcall si env expr nil nil)
    (ccc-p expr) (emit-ccc si env expr nil)
    (apply-p expr) (emit-apply si env expr)
    (do-p expr) (emit-do si env expr nil)
    (ffi-p expr) (emit-ffi-call si env expr nil)
    (err (make-string "Unknown expression: " expr))))

(def emit-immediate (expr)
  (movl (imm (imm-rep expr)) eax))

(def emit-fun-header (labl)
  ;(emit "    .globl " labl)
  (emit "    .type " labl ", @function")
  (label labl))

(def emit-fun-ret ()
  (emit "    ret"))

(def emit-header ()
  (emit "    .text"))

(def emit-init-entry (expr env)
  (let entry-label "__init"
    (globl entry-label)
    (emit-fun-header entry-label)
    (emit-expr (- wordsize) env expr)
    (emit-fun-ret)
    entry-label))

;(def expand-global-ref2 (expr)
;  (cond
;    ((atom expr) (if (global-var-p expr) `(get-symbol-value ',expr) expr))
;    ((setq-p expr) 
;     (let ((val (expand-global-ref (setq-val expr)))
;	   (var (setq-var expr)))
;       (if (global-var-p var)
;	   (list 'set-symbol-value (list 'quote var) val)
;	   (list setq-sym var val))))
;    (t (mapcar #'expand-global-ref expr))))

(def syntax-form-p (e)
  (and (consp e) (some (fn (x) (x e)) 
		       (list labelsp primcallp setq-p letp closurep 
			     ffi-p ccc-p funcallp apply-p labelcall-p 
			     do-p ifp lambdap quotep))))

(def emit-unit (expr env transformation)
  ; compiles an expression
  (let expr (transformation expr)
    (if (labelsp expr) 
      (emit-labels expr env (is transformation minimal-transform))
      (emit-init-entry expr env))))

(def decl-long (val)
  (emit "    .long " val))

(def decl-globl (lbl)
  (emit "    .globl " lbl))

(def emit-thread-trampoline ()
  (emit-fun-header "thread_trampoline")
  (emit-load (- wordsize) eax) ; get thread argument
  (movl (deref 0 eax) edi) ; get closure to call
  (movl (deref wordsize eax) esp) ; set new stack
  (movl esp main-stack-base)
  ; call the closure
  (movl (imm 0) eax)
  (emit-save (- wordsize) (imm nil-val))
  (emit-save (- (* 2 wordsize)) (imm frame-sentinel))
  (addl (- (* 2 wordsize)) esp)
  (call (unref-call (deref closureaddr-offset edi)))
  (subl (- (* 2 wordsize)) esp)
  (emit-fun-ret))

(set call-table-entries* (mkvec (+ basicmask 1) (cons call-table-err-lbl nil)))

(def add-call (basictag emitter)
  (let handler (unique-label)
    (vec-set call-table-entries* basictag 
             (cons handler (fn () (emit-fun-header handler) (emitter))))))

(def emit-call-table-handlers (from to)
  (if (< from to)
    (let entry (vec-ref call-table-entries* from)
      (if (cdr entry) ((cdr entry)))
      (emit-call-table-handlers (+ from 1) to))))

(def emit-call-table (from to)
  (if (< from to)
    (do
      (decl-long (car (vec-ref call-table-entries* from)))
      (emit-call-table (+ from 1) to))))

(def emit-static-routines ()
  ; emit code for static routines
  ;(emit-thread-trampoline)

  ; static call table
  ; 0 means invalid position
  ; init call table to error handlers
  (emit-call-table-handlers 0 (+ basicmask 1))
  (emit-call-table-error)
  (decl-globl call-table-lbl)
  (label call-table-lbl)
  (emit-call-table 0 (+ basicmask 1))

  (emit-static-type-check-routine "fx" fxmask fxtag nil)
  (emit-static-type-check-routine "ch" chmask chtag nil)
  (emit-static-type-check-routine "cell" cellmask celltag nil)
  (emit-static-type-check-routine "vec" vecmask vectag nil)
  (emit-static-type-check-routine "sym" symbolmask symboltag nil)
  (emit-static-type-check-routine "closure" closuremask closuretag nil)
  (emit-static-type-check-routine "str" fxmask strtag t)
  (emit-static-type-check-routine "float" nil floattag t)
  (emit-static-type-check-routine "continuation" nil continuation-tag t))

(add-call closuretag 
  (fn ()
    (jmp (unref-call (deref closureaddr-offset edi)))))

(add-call extendedtag
  (fn ()
    (withs (arg-si (fn-arg-si 0)
            free (next-si arg-si))
      (emit-exact-arg-count-check free nil 1)
      (movl edi eax) ; emit-is-str expects object in eax
      (emit-is-str free nil) ; check if it is a string
      (emit-load arg-si eax) ; get index
      (emit-is-fx free nil)
      (emit-bounds-check free extendedtag edi)
      (shrl (imm fxshift) eax) ; 1-byte
      (addl eax edi) ; sum offset
      (movb (deref strchar-offset edi) al) ; get value
      (movzbl al eax)
      (shll (imm chshift) eax)
      (op-orl (imm chtag) eax)
      (emit-fun-ret))))

(add-call vectag
  (fn ()
    (withs (arg-si (fn-arg-si 0)
            free (next-si arg-si))
      (emit-exact-arg-count-check free nil 1)
      (emit-load arg-si eax)
      (emit-is-fx free nil)
      (emit-bounds-check free vectag edi)
      (addl eax edi) ; sum offset
      (movl (deref (+ wordsize (- vectag)) edi) eax)
      (emit-fun-ret))))

(def do-n (f n)
  (if (> n 0)
    (do (f n) (do-n f (- n 1)))))

(def emit-call-table-error ()
  (decl-globl call-table-err-lbl)
  (emit-fun-header call-table-err-lbl)
  (movl edi ebx)
  (op-and (imm basicmask) ebx)
  (shll (imm fxshift) ebx)
  (emit-save (fn-arg-si 0) (imm (imm-rep closuretag)))
  (emit-save (fn-arg-si 1) ebx)
  (movl (imm 2) eax)
  (jmp '__type_error))

(def emit-program ()
  ;(emit-static-routines)

  ; entry function
  (globl "lisp_entry")
  (emit-fun-header "lisp_entry")

  ; save C context
  (movl (deref 4 esp) ecx)
  (movl ebx (deref 4 ecx))
  (movl esi (deref 16 ecx))
  (movl edi (deref 20 ecx))
  (movl ebp (deref 24 ecx))
  (movl esp (deref 28 ecx))
  (movl ecx esi)
  ; get heap base pointer
  (movl (deref 12 esp) ebp)
  ; get stack base pointer
  (movl (deref 8 esp) esp)
  ; init %edi at some meaningful value
  (movl (imm (imm-rep nil)) edi)
  ; call entry labels
  (emit-expr (- wordsize) (mk-empty-env) '(__load nil))
  (emit-save (- wordsize) (imm frame-sentinel))
  (addl (imm (- wordsize)) esp)
  (call "__init")
  (subl (imm (- wordsize)) esp)
  ; restore C context
  (movl esi ecx)
  (movl (deref 4 ecx) ebx)
  (movl (deref 16 ecx) esi)
  (movl (deref 20 ecx) edi)
  (movl (deref 24 ecx) ebp)
  (movl (deref 28 ecx) esp)
  (emit-fun-ret))

; Continuation structure:
; -----------------------------------------------------------------------
; | tag      | stack len (bytes) | ret. addr. | esp    | edi     | stack
; -----------------------------------------------------------------------
; | wordsize | wordsize          | wordsize   | w.size | w. size | ... 
; -----------------------------------------------------------------------

(install-primop '__restore-continuation
  (fn (si env cont-expr value)
    (emit-expr si env cont-expr)
    (emit-is-continuation si env)
    (emit-save si eax)
    (emit-expr (next-si si) env value)
    (movl eax edi) ; save value to return from continuation
    (emit-load si eax) ; get back continuation
    (emit-restore-stack)
    (movl (deref (+ (- extendedtag) (* 3 wordsize)) eax) esp) ; restore esp
    (movl edi ecx)
    (movl (deref (+ (- extendedtag) (* 4 wordsize)) eax) edi) ; restore cl. pt.
    (movl (deref (+ (- extendedtag) (* 2 wordsize)) eax) ebx) ; ret. addr.
    (movl ecx eax) ; return the value
    (jmp (unref-call ebx)))
  2 nil)

(def compile (stream-in stream-out program-p transform-fn)
  (with (e (cons 'do (readall stream-in))
         old stdout*)
    (set stdout* stream-out)
    (if (is transform-fn minimal-transform)
      (emit-static-routines))
    (emit-unit e (mk-empty-env) transform-fn)
    (if program-p
      (emit-program))
    (set stdout* old)))

(def compile-program (file-name out-dir)
  (withs (out-name (make-string out-dir "/" file-name ".s")
          in-file (open-file file-name 'in)
          out-file (open-file out-name 'out))
    (compile in-file out-file t transform-expr)
    (if (is (system (make-string "gcc -g -o " out-dir "/" file-name ".run start.c " out-name " " out-dir "/std.arc.so -ldl 2>err")) 0)
      t
      (err "Compilation error: see file err"))))

(def compile-unit (file-name out-dir)
  (withs (out-name (make-string out-dir "/" file-name ".s")
          in-file (open-file file-name 'in)
          out-file (open-file out-name 'out))
    (compile in-file out-file nil transform-expr)
    (if (is (system (make-string "gcc --shared -o " out-dir "/" file-name ".so " out-name " 2>err")) 0)
      t
      (err "Compilation error: see file err"))))

(def compile-minimal (file-name out-dir)
  (withs (out-name (make-string out-dir "/" file-name ".s")
          in-file (open-file file-name 'in)
          out-file (open-file out-name 'out))
    (compile in-file out-file nil minimal-transform)
    (if (is (system (make-string "gcc -g --shared -o " out-dir "/" file-name ".so consts.s runtime.c gc.c " out-name " 2>err")) 0)
      t
      (err "Compilation error: see file err"))))
