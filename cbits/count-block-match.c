#include <stdint.h> 

typedef unsigned __int128 uint128_t;


int count_block_match (uint8_t* hba, uint64_t block_first, int i) {
  const uint64_t lo_order_mask = 0x7F7F7F7F7F7F7F7Full;
  uint8_t* hba_ptr = hba + i;
  uint64_t* hba_big_ptr = (uint64_t*)hba_ptr;
  uint64_t one = hba_big_ptr[0] ^ block_first;
  uint64_t two = hba_big_ptr[1] ^ block_first;
  uint64_t three = hba_big_ptr[2] ^ block_first;
  uint64_t four = hba_big_ptr[3] ^ block_first;
  uint64_t five = hba_big_ptr[4] ^ block_first;
  uint64_t six = hba_big_ptr[5] ^ block_first;
  uint64_t seven = hba_big_ptr[6] ^ block_first;
  uint64_t eight = hba_big_ptr[7] ^ block_first;
  uint64_t final_one = ~((one + lo_order_mask) | lo_order_mask);
  uint64_t final_two = ~((two + lo_order_mask) | lo_order_mask);
  uint64_t final_three = ~((three + lo_order_mask) | lo_order_mask);
  uint64_t final_four = ~((four + lo_order_mask) | lo_order_mask);
  uint64_t final_five = ~((five + lo_order_mask) | lo_order_mask);
  uint64_t final_six = ~((six + lo_order_mask) | lo_order_mask);
  uint64_t final_seven = ~((seven + lo_order_mask) | lo_order_mask);
  uint64_t final_eight = ~((eight + lo_order_mask) | lo_order_mask);
  return __builtin_popcountll(final_one) + __builtin_popcountll(final_two)
          + __builtin_popcountll(final_three) + __builtin_popcountll(final_four) 
          + __builtin_popcountll(final_five) + __builtin_popcountll(final_six) 
          + __builtin_popcountll(final_seven) + __builtin_popcountll(final_eight);
}
