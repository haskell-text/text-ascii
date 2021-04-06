#include <string.h>
#include <stdint.h>
#include <stddef.h>

int find_first_match (uint8_t* hba, int hoff, int hlen, int w8, int start) {
  void* s = &(hba[start]);
  uint8_t* res = memchr (s, w8, hlen + hoff - start);
  if (res == NULL) {
    //search missed, so return off the end
    return hoff + hlen;
  }
  ptrdiff_t diff = res - hba;
  return diff;
}
