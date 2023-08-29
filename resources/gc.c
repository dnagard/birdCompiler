#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"

// These extern declarations allow us to refer to the variables you made global
// in your generated assembly file.

extern uint64_t *start_of_stack;
extern uint64_t *end_of_stack;
extern uint64_t *start_of_heap;
extern uint64_t *end_of_heap;
extern uint64_t *heap_cursor;

/*
  The following macros allow you to use "debugf" in place of "printf" for
  formatted debugging.  For instance, you can write

      debugf("Pointer %p changed to pointer %p.\n", old, new);

  to print in the same way you would use printf.  The advantage is that,
  depending upon which of the two macros you are using, the debugf call either
  turns into a printf call or is erased entirely.  Use debugf to print-debug
  your garbage collector and then disable printing when you need to perform your
  unit tests.
*/

// This macro disables all debugf statements.  (They become no-ops.)
#define debugf(fmt, ...) ;

// This macro enables all debugf statements.  (They become printf statements.)
 // #define debugf(fmt, ...)      \
  printf(fmt, ##__VA_ARGS__); \
  fflush(stdout)

// Function headers:
void followPointer(uint64_t *);
void mark();
uint64_t *forward();
void update();
void compact();

/**
 * This helper function will show the entire contents of your heap.  This output
 * can get very large if your heap is too big!
 */
void dump_heap()
{
  debugf("HEAP:\n");
  int c = 0;
  for (uint64_t *p = (uint64_t *)((uint64_t)start_of_heap & 0xFFFFFFFFFFFFFFFC);
       p < end_of_heap; p += 1)
  {
    if (c == 0)
    {
      debugf("%016" PRIx64 ":", p);
    }
    if (p >= start_of_heap)
    {
      debugf("    %016" PRIx64, *p);
    }
    else
    {
      debugf("            ");
    }
    c++;
    if (c == 4)
    {
      debugf("\n");
      c = 0;
    }
  }
  if (c != 0)
  {
    debugf("\n");
  }
}

void gc(int64_t desired_free)
{
  mark();
  uint64_t *nextHeapCursor = forward();

  uint64_t amountFreed = (uint64_t)(heap_cursor - nextHeapCursor);
  amountFreed = ((int64_t)amountFreed)*8;

  if (amountFreed < desired_free)
  {
    stopWithError(7);
  }

  update(nextHeapCursor);

  compact();

  heap_cursor = nextHeapCursor;
}

/* Mark section of the mark-compact algorithm */
void mark()
{

  uint64_t *i = start_of_stack;
  while (i >= end_of_stack)
  {
    uint64_t val = *i;
    // debugf("I'm here! Start of stack %p \n", start_of_stack);
    // debugf("I'm here! Val: pointer %p hex %x\n", val, val);
    if ((val & 0b11) == 0b01)
    {


      val ^= 1; /* Working here. Stuck on how to mark something. The compact algorithm seems to be working though and I do free memory, I just can't keep it */

      uint64_t *ptr = (uint64_t *)(uintptr_t)val;
      if ((ptr >= start_of_heap) && (ptr < heap_cursor))
      {
        followPointer(ptr);
      }
    }
    i--;
  }
}

void followPointer(uint64_t *p)
{
  uint64_t size = *p;
  if (p[1] == 0)
  {
    p[1] = 1;

    if ((size & 0x8000000000000000) == 0)
    {
      // Then it points to a tuple.
      for (int i = 0; i < (size); i++)
      {

        if ((p[i + 2] & 0b11) == 0b01)
        {

          uint64_t *ptr = (uint64_t *)(uintptr_t)(p[i + 2] - 1);
          if ((ptr >= start_of_heap) && (ptr < heap_cursor))
          {
            followPointer(ptr);
          }
        }
      }
    }
    else
    {
      // Then it points to a closure
      size ^= 0x8000000000000000;
      for (uint64_t i = 0; i < size; i++)
      {
        if ((p[i + 4] & 0b11) == 0b01)
        {
          uint64_t *ptr = (uint64_t *)(uintptr_t)(p[i + 4] - 1);
          if (ptr >= start_of_heap && ptr < heap_cursor)
          {
            followPointer(ptr);
          }
        }
      }
    }
  }
}

/* Forward part of the mark-compact algorithm */

uint64_t *forward()
{
  uint64_t *next_heap_object = start_of_heap;
  uint64_t *next_live_destination = start_of_heap;
  while (next_heap_object < heap_cursor)
  {
    uint64_t size = (*next_heap_object);

    if ((size & 0x8000000000000000) == 0) /* Then it's a tuple */
    {
      if (next_heap_object[1] != 0) /* Check GC word to see if it's reachable */
      {
        next_heap_object[1] = next_live_destination;
        next_live_destination += ((size + 2));
      }
      next_heap_object += ((size + 2));
    }
    else /* Then it's a closure */
    {
      size ^= 0x8000000000000000;
      if (next_heap_object[1] > 0) /* Check GC word to see if it's reachable */
      {
        next_heap_object[1] = (uint64_t)next_live_destination;
        next_live_destination += ((size + 4));
      }
      next_heap_object += ((size + 4));
    }
  }
  return next_live_destination;
}

/* The update section. Iterate over the stack and the heap to update all old
pointers to the new ones now that the garbage collector has run */
void update()
{

  uint64_t *i = start_of_stack;
  while (i >= end_of_stack)
  {
    uint64_t val = *i;

    if ((val & 0b11) == 0b01)
    {
      val ^= 1;
      uint64_t *ptr = (uint64_t *)(uintptr_t)val;
      if ((ptr >= start_of_heap) && (ptr < heap_cursor))
      {
        *i = ptr[1] + 1;
      }
    }
    i--;
  }

  uint64_t *next_heap_object = start_of_heap;
  while (next_heap_object < heap_cursor)
  {
    if (((*next_heap_object & 0b11) == 1) && (*next_heap_object >= start_of_heap) && (*next_heap_object < heap_cursor))
    {
      uint64_t *newPointer = (uint64_t *)(uintptr_t)((*next_heap_object) - 1);

      *next_heap_object = newPointer[1] + 1;
    }
    next_heap_object++;
  }
}

void compact()
{
  uint64_t *next_heap_object = start_of_heap;
  while (next_heap_object < heap_cursor)
  {
    uint64_t size = (*next_heap_object);
    if ((size & 0x8000000000000000) == 0) /* Then it's a tuple */
    {
      if (next_heap_object[1] != 0) /* Check GC word to see if it's reachable */
      {
        uint64_t unprocessedAddr = next_heap_object[1];
        uint64_t *newAddr = (uint64_t *)(uintptr_t)unprocessedAddr;
        next_heap_object[1] = 0;
        for (uint64_t i = 0; i < size + 2; i++)
        {
          newAddr[i] = next_heap_object[i];
        }
      }
      next_heap_object += ((size + 2));
    }
    else /* Then it's a closure */
    {
      size ^= 0x8000000000000000;
      if (next_heap_object[1] > 0) /* Check GC word to see if it's reachable */
      {
        uint64_t *newAddr = (uint64_t *)(uintptr_t)next_heap_object[1];
        next_heap_object[1] = 0;
        for (uint64_t i = 0; i < size + 4; i++)
        {
          newAddr[i] = next_heap_object[i];
        }
      }
      next_heap_object += ((size + 4));
    }
  }
}