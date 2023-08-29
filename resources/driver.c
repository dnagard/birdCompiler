#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include "printer.h"

int64_t bird_main(int64_t* ptr, int64_t* endPtr) asm("bird_main");

int main(int argc, char** argv) {
  int size = 1000000;
  int64_t* heap = (int64_t*)malloc(sizeof(int64_t)*size);
  int64_t* endHeap = (int64_t*) ((int64_t) heap + sizeof(int64_t)*size);
  if (heap == NULL) {
        printf("Memory not allocated.\n");
        exit(0);
    }
  int64_t result = bird_main(heap, endHeap);
  printValue(("%"PRId64"\n", result));
  return 0;
}