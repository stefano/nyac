/*
  Copyright (c) 2008 Dissegna Stefano
  Released under the terms of the GNU LGPL
*/

#include <stdio.h>
#include "runtime.h"

ptr lisp_entry(context *c, char *stack_base, char *heap_base);

int main(int argc, char **argv)
{
  int stack_size = 1600 * 4096;
  int heap_size = 16 * 4096;

  char *stack_top = allocate_protected_space(stack_size);
  char *stack_base = stack_top + stack_size;

  main_stack_base = (ptr*)stack_base;

  //  printf("%p\n", main_stack_base);

  __const_roots = (ptr**)allocate_protected_space(heap_size);
  __const_roots_top = 0;

  char *heap = init_main_gc(stack_base);

  context c;
  
  print_ptr(lisp_entry(&c, stack_base, heap));
  printf("\n");

  /*  print_consts();*/

  deallocate_protected_space((char*)__const_roots, heap_size);
  deallocate_protected_space(stack_top, stack_size);
  //deallocate_protected_space(heap, heap_size);

  return 0;
}
