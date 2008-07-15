/*
  Copyright (c) 2008 Dissegna Stefano
  Released under the terms of the GNU LGPL
*/

#include "runtime.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define DEBUG 0
//#define FORCE_GC 
#define INITIAL_HEAP_SIZE (16 * 4096)
#define MIN_HEAP_SIZE INITIAL_HEAP_SIZE

void test_mem(char *mem, unsigned int n, int content)
{
  unsigned int i;
  for (i = 0; i<n;i+=4)
    if (*((ptr*)(mem+i))==content)
      {
	printf("found! :%p\n", mem+i);
	//exit(0);
      }
}

typedef struct _gc
{
  char *heap1; /* heap to be copied */
  //char *heap1_next; /* next free adress in heap1 */
  unsigned int heap1_size;

  char *heap2; /* new heap */
  unsigned int heap2_size;
  //  int heap2_start;
  unsigned int heap2_top;

  ptr *stack; /* stack pointer */

  unsigned int next_alloc_size; /* size of the next heap */
} gc;

char* gc_init(gc *g, char *stack)//, char *croots)
{
  g->heap1 = allocate_protected_space(INITIAL_HEAP_SIZE);
  //g->heap1_next = g->heap1;
  g->heap1_size = INITIAL_HEAP_SIZE;
  //  g->heap1_top = 0;
  g->heap2 = 0;
  g->heap2_size = 0;
  //  g->heap2_start = 0;
  g->heap2_top = 0;
  g->stack = (ptr*)stack;
  //g->stack_top = 0;
  //g->croots = croots;
  //g->croots_top = 0;

  g->next_alloc_size = INITIAL_HEAP_SIZE * 2;

  return g->heap1;
}

static gc main_gc;

char* init_main_gc(char *stack)
{
  return gc_init(&main_gc, stack);
}


int check_broken_heart(ptr pt, unsigned int type)
{
  switch(type)
    {
    case extended_tag:
    case cell_tag:
    case symbol_tag:
    case closure_tag:
    case vec_tag:
      if (is_bh(pt, type))
	return 1;
      break;
    }

  return 0;
}

int round_at_boundary(int n, int boundary)
{
  return  ((n+(boundary-1))/boundary)*boundary;
  //return (n+boundary)&(-boundary);
}

void alloc_n(gc *g, int n)
{
  g->heap2_top += round_at_boundary(n, 2*wordsize);
}

void copy_n(gc *g, char *from, int n)
{
  //  printf("old top is %d\n", g->heap2_top);
  //printf("copying %d bytes from %p to %p\n", n, from, g->heap2+g->heap2_top);
  //  printf("In copy_n 0\n");
  //test_mem(from, n, 6);
  memcpy(g->heap2+g->heap2_top, from, n);
  g->heap2_top += round_at_boundary(n, 2*wordsize);
  //printf("In copy_n\n");
  //test_mem(g->heap2, g->heap2_top, 6);
  //printf("new top is %d\n", g->heap2_top);
}

//void copy_n_with_sentinel(gc *g, char *from, unsigned int n, int sentinel)
//{
  //  printf("copying from %p to %p, %d\n", from, g->heap2+g->heap2_top+wordsize, n);
  //*((ptr*)(g->heap2+g->heap2_top)) = sentinel;
//memcpy((g->heap2+g->heap2_top)+2*wordsize, from, n);
//g->heap2_top += round_at_boundary(n+2*wordsize, 2*wordsize);
//}

void put_sentinel(gc *g, unsigned int sent)
{
  //printf("put_sent 0\n");
  //test_mem(g->heap2, g->heap2_top, 6);
  *((ptr*)(g->heap2+g->heap2_top)) = sent;
  g->heap2_top += 2*wordsize;
  //printf("put_sent\n");
  //test_mem(g->heap2, g->heap2_top, 6);
}

ptr resolve_broken_heart(ptr pt, unsigned int type)
{
  if (check_broken_heart(pt, type))
    return resolve_broken_heart(bh_addr(pt, type)|type, type);
  else
    return pt;
}

ptr copy_ptr(gc *g, ptr pt)
{
  unsigned int type = basic_type(pt);
  if (check_broken_heart(pt, type))
    {
      //        printf("br %x %x %x\n", pt, type, bh_addr(pt, type));
      return bh_addr(pt, type)|type;
      //return resolve_broken_heart(pt, type);
    }
  //  printf("not br %x\n", pt);
  switch(type)
    {
    case extended_tag:
      {
	unsigned int etype = extended_type(pt);
	if ((etype&fx_mask)==str_tag)
	  {
	    put_sentinel(g, str_sentinel);
	    char *addr = ((char*)pt)-extended_tag;
	    char *new_addr = g->heap2+g->heap2_top;
	    copy_n(g, addr, wordsize+str_len(pt));
	    /*		print_ptr(pt);
			printf("str %p %d: ", new_addr, str_len(pt));
			print_ptr((ptr)(new_addr)|str_tag);
			printf("\n");*/
	    mark_bh(pt, extended_tag);
	    bh_addr(pt, extended_tag) = (ptr)new_addr;
	    return ((ptr)new_addr)|extended_tag;
	  }
	else
	  {
	    switch(etype)
	      {
	      case float_tag:
		{
		  char *addr = ((char*)pt)-extended_tag;
		  char *new_addr = g->heap2+g->heap2_top;
		  copy_n(g, addr, 4*wordsize);
		  mark_bh(pt, extended_tag);
		  bh_addr(pt, extended_tag) = (ptr)new_addr;
		  //printf("fl %p\n", bh_addr(
		  return ((ptr)new_addr)|extended_tag;
		}
		break;
	      default:
		printf("GC: don't know how to handle unknown type\n");
		exit(1);
		break;
	      }
	  }
      }
      break;
    case cell_tag:
      {
	char *new_addr = g->heap2+g->heap2_top;
	copy_n(g, ((char*)pt)-cell_tag, 2*wordsize);
	mark_bh(pt, cell_tag);
	bh_addr(pt, cell_tag) = (ptr)new_addr;
	return ((ptr)new_addr)|cell_tag;
      }
      break;
    case symbol_tag:
      {
	//printf("symbol    : %x %x %x %x\n", sym_ref(pt, 0), sym_ref(pt, 1), sym_ref(pt, 2), sym_ref(pt, 3));
	//printf("before: %x\n", sym_ref(pt, 1));
	char *new_addr = g->heap2+g->heap2_top;
	copy_n(g, ((char*)pt-symbol_tag), 4*wordsize);
	//g->heap2_top+=4;
	//test_mem(g->heap2, g->heap2_top, 6);
	//	if (DEBUG)
	  {
	    //ptr pn = (ptr)new_addr|symbol_tag;
	    //printf("new symbol: %x %x %x %x\n", sym_ref(pn, 0), sym_ref(pn, 1), sym_ref(pn, 2), sym_ref(pn, 3));
	    fflush(stdout);
	  }
	  //	       printf("after: %x\n", sym_ref(((ptr)new_addr)|symbol_tag, 1));
	mark_bh(pt, symbol_tag);
	bh_addr(pt, symbol_tag) = (ptr)new_addr;
	return ((ptr)new_addr)|symbol_tag;
      }
      break;
    case closure_tag:
      {
	//printf("%d - %d\n", g->heap2_start, g->heap2_top);
	put_sentinel(g, closure_sentinel);
	char *new_addr = g->heap2+g->heap2_top;
	//printf("len %d\n", closure_len(pt));
	copy_n(g, ((char*)pt)-closure_tag, (2+closure_len(pt))*wordsize);
	mark_bh(pt, closure_tag);
	bh_addr(pt, closure_tag) = (ptr)new_addr;
	//printf("addr %p\n", bh_addr(pt, closure_tag));
	return ((ptr)new_addr)|closure_tag;
      }
      break;
    case vec_tag:
      {
	char *new_addr = g->heap2+g->heap2_top;
	copy_n(g, ((char*)pt-vec_tag), (vec_len(pt)+1)*wordsize);
	mark_bh(pt, vec_tag);
	bh_addr(pt, vec_tag) = (ptr)new_addr;
	//printf("v %x\n", bh_addr(pt, vec_tag));
	return ((ptr)new_addr)|vec_tag;
      }
      break;
    default:
      {
	//printf("default: %xk\n", pt);
	  return pt;
      }
      break;
    }
}

unsigned int skip(gc *g, ptr pt, unsigned int start)
{
  //  printf("In skip\n");
  //if (pt==6)
  //{
  //  printf("!!\n");
      //exit(1);
  //}
  switch(pt)
    {
    case str_sentinel:
      return start+2*wordsize+round_at_boundary(str_len((g->heap2+start+2*wordsize)+extended_tag)+wordsize, 2*wordsize);
      break;
    case closure_sentinel:
      return start+2*wordsize+2*wordsize;
      break;
    case float_tag:
      {
	//printf("here\n");
	return start+4*wordsize;
      }
      break;
    default:
      return start+wordsize;
      break;
    }
}

ptr do_copy(gc *g, ptr pt)
{
  unsigned int start = g->heap2_top;
  if (DEBUG)
    {
      printf("copying top %p: ", (void*)pt);
      print_ptr(pt);
      printf("\n");
    }
  pt = copy_ptr(g, pt);
  /*
  if (pt==6)
    {
    printf("returned 6\n");
    exit(1);
    }*/
  while (start<g->heap2_top)
    {
      if (DEBUG)
	{
	  printf("from %p, end is %p\n", g->heap2+start, g->heap2+g->heap2_top);
	  ptr p = *((ptr*)(g->heap2+start));
	  printf("copying: ");
	  fflush(stdout); 
	  if (p==str_sentinel)
	    {
	      printf("tagged: ");
	      print_ptr(((ptr)(g->heap2+start+2*wordsize))|extended_tag);
	    }
	  else
	    if (p==closure_sentinel)
	      {
		printf("tagged: ");
		print_ptr(((ptr)(g->heap2+start+2*wordsize))|closure_tag);
	      }
	    else
	      print_ptr(p); 
	  printf("\n");
	}

      //printf("c: %x\n",*((ptr*)(g->heap2+start)));
      *((ptr*)(g->heap2+start)) = copy_ptr(g, *((ptr*)(g->heap2+start)));
      //printf("c: %x\n",*((ptr*)(g->heap2+start))); 
      //fflush(stdout);
      start = skip(g, *((ptr*)(g->heap2+start)), start);
    }

  if (DEBUG)
    {
      printf("returning: ");
      print_ptr(pt);
      printf("\n");
    }

  return pt;
}

unsigned int roundp2(unsigned int n)
{
  int i=0;
  while(n>0)
    {n>>=1;i++;}
  return 1<<i;
}

/* checks if heap has space for n bytes, if not expands it. 
   doesn't allocate the space, returns new heap pointer */
char* expand_heap(gc *g, char *heap_pt, int stack_top, unsigned int n)
{
#ifndef FORCE_GC
  if (heap_pt - g->heap1 + n >= g->heap1_size) /* not enough space */
#endif
    {
      //if (DEBUG)
	//{
      //printf("\n\nCollection begin\n");
	  //  fflush(stdout);
	  //}
      unsigned int r = roundp2(n+heap_pt-g->heap1);
      //if (g->next_alloc_size<r)
      g->next_alloc_size = r;
	//printf("Requested %u, allocated %u\n", n+heap_pt-g->heap1, g->next_alloc_size);
      g->heap2 = allocate_protected_space(g->next_alloc_size);//INITIAL_HEAP_SIZE);//g->heap1_size * 2);
      //      test_mem(g->heap2, g->next_alloc_size);
      //memset(g->heap2, 0, g->next_alloc_size);
      g->heap2_top = 0;
      //g->heap2_start = 0;
      unsigned int heap2_size = g->next_alloc_size;//INITIAL_HEAP_SIZE;//g->heap1_size * 2;
      int i;
      //printf("const top: %d\n", __const_roots_top);
      for (i = 0; i<__const_roots_top; i++)
	{
	  if (DEBUG)
	    {
	      printf("copying const (%d): ", i);
	      print_ptr(*(__const_roots[i]));
	      printf("\n");
	      fflush(stdout);
	    }
	  *(__const_roots[i]) = do_copy(g, *(__const_roots[i]));
	  //printf("const after: "); print_ptr(*(__const_roots[i])); printf("\n");
	}
      if (DEBUG)
	printf("stack top: %d\n", stack_top);
      /* C return adress at the base of the stack */
      ptr previous = nil_val; /* previuos element in the stack */
      for (i = -1; i>stack_top; i--)
	{	      
	  if (DEBUG)
	    {
	      printf("stack at %d %x:\n ", i, g->stack[i]);
	      fflush(stdout);
	      if (previous==frame_sentinel)
		printf("#<return adress: %p>", (void*)g->stack[i]);
	      else
		print_ptr(g->stack[i]);
	      printf("\n");
	    }
	  if (previous!=frame_sentinel) /* g->stack[i] isn't a ret. addr. */
	    {
	      g->stack[i] = do_copy(g, g->stack[i]);
	      previous = g->stack[i];
	    }
	  else
	    previous = nil_val; /* ret. addr. could have sentinel value */
	}
      deallocate_protected_space(g->heap1, g->heap1_size);
      g->heap1 = g->heap2;
      g->heap1_size = heap2_size;
      if (g->heap2_top < g->heap1_size/4) /* too big */
      	{
	  if (g->heap2_top>2*MIN_HEAP_SIZE)
	    g->next_alloc_size /= 2;
      //  printf("next will shrink\n");
	}
      else
	if (g->heap2_top > 3*(g->heap1_size/4)) /* too small */
	  {
	    g->next_alloc_size *= 2;
	    //  printf("next will grow\n");
	  }
      char *new_heap_pt = g->heap2 + g->heap2_top;
      g->heap2 = 0;
      //g->heap2_size = 0;
      g->heap2_top = 0;

      return new_heap_pt;
    }
#ifndef FORCE_GC
  else
#endif
    {
      return heap_pt;
    }
}

//int count = 0;

char* main_expand_heap(char *heap_pt, unsigned int stack_top, unsigned int n)
{
  int top = (int)stack_top - (int)(main_gc.stack);
  //  printf("%p %d %d\n", heap_pt, top, n);

  //  print_backtrace((unsigned int)main_gc.stack, stack_top);

  //  if (DEBUG)
  //printf("\n\nallocating %d bytes\n", n);

  //  printf("%d\n", count);
  //count++;
  //printf("Begin, stack top is %d\n", top/wordsize);
    //print_backtrace(stack_top, nil_val);
  
 //  test_mem(main_gc.heap1, heap_pt-main_gc.heap1, 6);

  //  printf("%p == %p\n", main_gc.heap1_next, heap_pt);

  return expand_heap(&main_gc, /*main_gc.heap1_next*/heap_pt, top/wordsize, n);
  //main_gc.heap1_next = ret+n;

  //return ret;
}
