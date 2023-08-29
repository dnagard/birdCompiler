#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

#include "error.h"

void stopWithError(int64_t type) {
  switch (type) {
    case 1:
      printf("Runtime error 1: Expected an int.\n");
      break;
    case 2:
      printf("Runtime error 2: Expected a boolean.\n");
      break;
    case 3: 
      printf("Runtime error 3: Expected a tuple.\n");
      break;
    case 4: 
      printf("Runtime error 4: Indexing out of range.\n");
      break;
    case 5: 
      printf("Runtime error 5: Expected closure.\n");
      break;
    default:
      printf("Unknown error %"PRId64" occurred.\n", type);
      break;
  }
  exit(type);
}
