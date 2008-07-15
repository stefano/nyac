/*
  Copyright (c) 2008 Dissegna Stefano
  Released under the terms of the GNU LGPL
*/

#ifndef RUNTIME_H
#define RUNTIME_H

#define wordsize 4

#define nil_val 0x2F
#define t_val 0x6F

#define fx_mask 0x3
#define fx_tag 0x0
#define fx_shift 2

#define ch_shift 8
#define ch_mask 0xFF
#define ch_tag 0x0F

#define cell_mask 0x7
#define cell_tag 0x1

#define extended_tag 0x6

#define str_tag fx_tag

#define vec_tag 0x5

#define closure_tag 0x2
#define closure_len_offset (-closure_tag)
#define closure_addr_offset (wordsize-closure_tag)

#define symbol_tag 0x3

#define basic_mask 0x7
#define extended_mask 0xFF

#define float_tag 0xCF

#define frame_sentinel 0xFF

#define broken_heart_tag 0x8F

/* GC puts these tags in the heap before a string or a closure */
#define str_sentinel 0x67
#define closure_sentinel 0x27

typedef unsigned int ptr;

#define basic_type(x) ((x)&basic_mask)
#define extended_type(x) (*((ptr*)(((char*)(x))-extended_tag)))

#define fxtoi(x) (((int)(x))>>fx_shift)
#define itofx(x) ((x)<<fx_shift)

#define chtoc(x) ((x)>>ch_shift)
#define ctoch(x) (((x)<<ch_shift)|ch_tag)

#define car(x) (*((ptr*)(((char*)(x))-cell_tag)))
#define cdr(x) (*((ptr*)(((char*)(x))-cell_tag+wordsize)))

#define str_len(str) (fxtoi(*((ptr*)(((char*)(str))-extended_tag))))
#define str_ref(str, i) (((char*)(((char*)(str))-extended_tag))[wordsize+(i)])

#define vec_len(v) (fxtoi(*((ptr*)(((char*)(v))-vec_tag))))
#define vec_ref(v, i) (((ptr*)(((char*)(v))-vec_tag))[1+(i)])

#define closure_len(c) (fxtoi(*((ptr*)(((char*)(c))+closure_len_offset))))
#define closure_ref(c, i) (((ptr*)(((char*)(c))-closure_tag))[2+(i)])

/* i == 0 -> string; i == 1 -> global value; i == 2 -> plist */
#define sym_ref(s, i) (((ptr*)(((char*)(s))-symbol_tag))[(i)])

#define mark_bh(pt, tag) ((*((ptr*)(((char*)(pt))-(tag)))) = broken_heart_tag)
#define is_bh(x, tag) (((*((ptr*)(((char*)(x))-(tag))))&extended_mask)==broken_heart_tag)
#define bh_addr(x, tag) (((*((ptr*)(((char*)(x))-(tag)+wordsize)))))

#define flval(x) (*((double*)(((char*)(x))-extended_tag+wordsize)))

typedef struct _context
{
  void *eax; /* 0 scratch */
  void *ebx; /* 4 preserve */
  void *ecx; /* 8 scratch */
  void *edx; /* 12 scratch */
  void *esi; /* 16 preserve */
  void *edi; /* 20 preserve */
  void *ebp; /* 24 preserve */
  void *esp; /* 28 preserve */
} context;

extern ptr **__const_roots;
extern int __const_roots_top;

char* allocate_protected_space(int size);
void deallocate_protected_space(char *p, int size);

char* init_main_gc(char *stack);

int check_broken_heart(ptr pt, unsigned int type);

void print_ptr(ptr x);

void print_backtrace(ptr *stack_top, ptr current_closure_pt);

void* full_load(ptr filename);

extern ptr *main_stack_base;

#endif /* RUNTIME_H */
