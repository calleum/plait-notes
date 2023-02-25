#include <uthash.h>
#include <stdio.h>

int CURRENT_ID = 0;

/**
*  This reproduces a bitshifting overflow problem. Depends on 
*  troydhanson.github.io/uthash for the hashset. 
* */
struct object_id {
  int id;            /* we'll use this field as the key */
  UT_hash_handle hh; /* makes this structure hashable */
};

struct object_id *objects = NULL;

void add_object_id(int temp_id) {
  struct object_id *s;

  s = malloc(sizeof *s);
  s->id = temp_id;
  HASH_ADD_INT(objects, id, s);
}

int find_object_id(int object_id) {
  struct object_id *s;

  HASH_FIND_INT(objects, &object_id, s);
  if (s == NULL) {
    return -1;
  }
  return s->id;
}

int next_int() {
  return CURRENT_ID++;
}
// From Cayenne
// github.com/apache/cayenne/blob/0ba08199792ba298ccb1d79ff26060d0e5369841/cayenne-server/src/main/java/org/apache/cayenne/util/IDUtil.java
void sequence(unsigned char *bytes) {
  int BITMASK_0 = 0xff;
  int BITMASK_1 = 0xff << 8;
  int BITMASK_2 = 0xff << 16;
  // bytes 0..2 - incrementing #
  // bytes 3..5 - timestamp high bytes
  // bytes 6..7 - IP address
  int nextInt = next_int();

  bytes[0] = ((nextInt & (0xff << 16)) >> 16);
  bytes[1] = ((nextInt & (0xff << 8)) >> 8);
  bytes[2] = (nextInt & 0xff);

  // append 3 high bytes of timestamp

  long t = 1677303363;

  bytes[3] = ((t & BITMASK_2) >> 16);
  bytes[4] = ((t & BITMASK_1) >> 8);
  bytes[5] = (t & BITMASK_0);

  // append 2 last bytes of IP address
  bytes[6] = 0;
  bytes[7] = 1;
}

int main(void) {
  unsigned char key[8];

  int i;
  int k;
  printf("Running...\n");
  for (k = 0; k < 20000000; k++) {
      // printf("Round [%d]...\n", k);
    int temp_id = 0;
    sequence(key);
    for (i = 3; i >= 0; i--) {
      temp_id = key[i] << (8 * i) | temp_id;
      /* printf("temp_id: [%d],\titeration: [%d],\tkey[i]: [%d]\n", temp_id, i,
             key[i]); */
    }
    if (find_object_id(temp_id) == temp_id) {
      printf("temp_id: [%d] was already in the hashset!! Would have cause a "
             "constraint violation at round %d with CURRENT_ID %d.\n",
             temp_id, k, CURRENT_ID);
      return 1;
    }
    add_object_id(temp_id);
  }

  return 0;
}
