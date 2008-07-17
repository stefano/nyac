/*
  Copyright (c) 2008 Dissegna Stefano
  Released under the terms of the GNU LGPL
*/

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dlfcn.h>

#include "runtime.h"

void print_ptr(ptr x)
{
  //  printf("pr: %x\n", x);

  unsigned int type = basic_type(x);
  if (check_broken_heart(x, type))
    {
      printf("#<broken heart %p>", (void*)bh_addr(x, type));
      return;
    }
  switch(type)
    {
    case 4:
    case fx_tag:
      printf("%d", fxtoi(x));
      break;
    case cell_tag:
      {
	ptr c;
	printf("(");
	for (c = x; basic_type(c)==cell_tag; c = cdr(c))
	  {
	    print_ptr(car(c));
	    if (basic_type(cdr(c))==cell_tag)
	      printf(" ");
	  }
	if (c!=nil_val)
	  {
	    printf(" . ");
	    print_ptr(c);
	  }
	printf(")");
      }
      break;
    case vec_tag:
      {
	int i;
	printf("#(");
	for (i = 0; i<vec_len(x); i++)
	  {
	    print_ptr(vec_ref(x, i));
	    printf(" ");
	  }
	printf(")");
      }
      break;
    case closure_tag:
      {
	int i;
	printf("#<function: ");
	print_ptr(closure_ref(x, 0));
	printf(", address %p, closes (", (void*)closure_ref(x, -1));
	fflush(stdout);
	for (i = 1; i<closure_len(x); i++)
	  {
	    print_ptr(closure_ref(x, i));
	    if (i<closure_len(x)-1)
	      printf(" ");
	  }
	printf(")>");
      }
      break;
    case extended_tag:
      {
	unsigned int etype = extended_type(x);
	if ((etype&fx_mask)==str_tag)
	  {
	    int i;
	    printf("\"");
	    for (i = 0; i<str_len(x); i++)
	      printf("%c", str_ref(x, i));
	    printf("\"");
	  }
	else
	  {
	    switch(etype)
	      {
	      case float_tag:
		printf("%.18F", flval(x));
		break;
	      case continuation_tag:
		printf("#<continuation>");
		break;
	      default:
		printf("#<unknown %x>", x);
		break;
	      }
	  }
      }
      break;
    case symbol_tag:
      {
	int i;
	//printf("<contents: %p %d>", (char*)sym_ref(x, 0), str_len(sym_ref(x, 0)));
	ptr s = sym_ref(x, 0);
	//printf("#<sym: ");
	//print_ptr(s);
	//printf(", ");/* print_ptr(sym_ref(x, 1));*/ printf(", ");
	//print_ptr(sym_ref(x, 2));
	//printf(">");
	for (i = 0; i<str_len(s); i++)
	  printf("%c", str_ref(s, i));
      }
      break;
    default:
      switch(x)
	{
	case nil_val:
	  printf("nil");
	  break;
	case t_val:
	  printf("t");
	  break;
	case frame_sentinel:
	  printf("#<frame sentinel>");
	  break;
	default:
	  if ((x&extended_mask)==ch_tag)
	    {
	      if (chtoc(x)=='\n')
		printf("#\\Newline");
	      else
		printf("#\\%c", chtoc(x));
	    }
	  else
	    printf("#<unknown: %p>", (void*)x);
	  break;
	}
      break;
    }
}

void print_backtrace(ptr *stack_top, ptr current_closure_pt)
{
  unsigned int top = 
    ((unsigned int)main_stack_base-(unsigned int)stack_top)/wordsize;

  printf("Within:\n\t");
  print_ptr(current_closure_pt);
  printf("\nBacktrace:\n");
  unsigned int i;
  /* from top-3 to skip printing error handler locals */
  for (i = top-3; i>2; i--)
    {
      if (main_stack_base[-i]==frame_sentinel)
	printf("\t -- frame end --\n");
      else
	{
	  if (i>3 && main_stack_base[-(i-1)]==frame_sentinel)
	    {}//	printf("#<return adress: %p>", (void*)main_stack_base[-i]);
	  else
	    {
	      printf("\t");
	      print_ptr(main_stack_base[-i]);
	      printf("\n");
	    }
	}
    }
}

char* allocate_protected_space(int size)
{
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char *p = mmap(0, aligned_size + 2 * page, PROT_READ | PROT_WRITE,
		 MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);
  if (p==MAP_FAILED)
    {
      fprintf(stderr, "Cannot allocate stack space\n");
      exit(-1);
    }
  status = mprotect(p, page, PROT_NONE);
  if (status!=0)
    {
      fprintf(stderr, "Cannot protect stack space\n");
      exit(-1); 
    }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status!=0)
    {
      fprintf(stderr, "Cannot protect stack space\n");
      exit(-1); 
    }

  return p + page;
}

void deallocate_protected_space(char *p, int size)
{
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;

  status = munmap(p - page, aligned_size + 2 * page);
  if (status!=0)
    {
      fprintf(stderr, "Cannot deallocate stack space\n");
      exit(-1);       
    }
}

char* strtocstr(ptr str)
{
  int len = str_len(str);
  char *res = malloc(len+1);
  int i;
  for (i = 0; i<len; i++)
    res[i] = str_ref(str, i);
  res[len] = '\0';
  return res;
}

ptr a_open(ptr path, ptr mode)
{
  char *s = strtocstr(path); 
  int res = open(s, fxtoi(mode), S_IRWXU);
  free(s);
  return itofx(res);
}

ptr a_write(ptr fd, ptr buf, ptr count)
{
  int res = 
    write(fd>>fx_shift, (char*)(buf-extended_tag+wordsize), count>>fx_shift);

  return res<<fx_shift;
}

ptr a_read(ptr fd, ptr buf, ptr count)
{
  int res = 
    read(fd>>fx_shift, (char*)(buf-extended_tag+wordsize), count>>fx_shift);

  return res<<fx_shift;
}

ptr a_system(ptr cmd)
{
  char *s = strtocstr(cmd);
  int res = system(s);
  free(s);
  return res<<fx_shift;
}

ptr a_bits1(ptr p)
{
  double d = flval(p);
  return *((unsigned int*)(&d));
}

unsigned int a_bits2(double d)
{
  return *((unsigned int*)(((char*)(&d))+4));
}

/* string handling */

int str_eq(ptr s1, ptr s2)
{
  unsigned int i;
  unsigned int len = str_len(s1);

  if (len!=str_len(s2))
    return 0;

  for (i = 0; i<len; i++)
    if (str_ref(s1,i)!=str_ref(s2,i))
      return 0;

  return 1;
}

void* load_file(ptr filename)
{
  char *fname = strtocstr(filename);
  void *handle = dlopen(fname, RTLD_LAZY);
  free(fname);
  if (!handle)
    return 0;
  return dlsym(handle, "__init");
}

void* load_std()
{
  void *handle = dlopen("std.arc.so", RTLD_NOW|RTLD_GLOBAL);
  if (!handle)
    return 0;
  return dlsym(handle, "__init");
}

void* full_load(ptr filename)
{
  char *fname;
  if (((unsigned int)filename)==nil_val)
    fname = "std.arc.so";
  else
    fname = strtocstr(filename);
  void *handle = dlopen(fname, RTLD_NOW|RTLD_GLOBAL);
  if ((unsigned int)filename!=nil_val)
    free(fname);
  if (!handle)
    return 0;
  return dlsym(handle, "__init");
}

/*
  allocates space in the heap and copies the current stack in reverse order
  into it
*/
void stack_copy_rev(unsigned int stack_top)
{
  unsigned int top = stack_top/wordsize;
  ptr *dest = main_expand_heap2(stack_top, stack_top-main_stack_base);
  ptr *src = main_stack_base+top;
  while (src!=main_stack_base)
    *dest++ = *src--;
}

/*
  takes a stack saved in the heap and overwrites with it the current stack
  len is in bytes
*/
void restore_stack_from(ptr *from, unsigned int len)
{
  ptr *dest = main_stack_base;
  ptr *end = ((char*)dest)-len;
  while (dest!=end)
    *dest-- = *from++;
}
