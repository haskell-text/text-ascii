#define _GNU_SOURCE
#include <string.h>
#include <stdint.h>
#include <stddef.h>

int find_first_block (uint8_t* nba, int noff, int nlen, uint8_t* hba, int hoff, int hlen) {
  void* needle = &(nba[noff]);
  void* haystack = &(hba[hoff]);
  uint8_t* res = memmem (haystack, hlen, needle, nlen);
  if (res == NULL) {
    //search missed, so return off the end
    return hoff + hlen;
  }
  ptrdiff_t diff = res - hba;
  return diff;
}
