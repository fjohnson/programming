#include <stdio.h>
#include <stdlib.h>

/*A short example describing the use of flexible arrays*/

struct interesting{
  char b;
  char c[];
};

int main(int argc, char *argv){
  struct interesting c;

  printf("Size of interesting structure is %lu\n", sizeof(struct interesting));

  struct interesting *ptr = (struct interesting *) malloc(sizeof(struct interesting) + 4);
  ptr->b = 4;
  ptr->c[0] = 'a';
  ptr->c[1] = 'b';
  ptr->c[2] = 'c';
  ptr->c[3] = 0;
  printf("Contents of flexible array %s\n",ptr->c);

  return 0;
}
