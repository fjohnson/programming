#include <stdlib.h>
#include <stdio.h>
#include <ftw.h>

/* Sunday April 24 2011
   A short example showing how to use the ftw function which traverses
   a file tree and calls the callback on each file.
*/
int callback (const char *filename, const struct stat *strct, int type){
  if(type == FTW_F || type == FTW_D){
    printf("%s\n",filename);
  }
  return 0;
}

int main(int argc, char **argv){
  if(argc < 2) return EXIT_FAILURE;
  ftw(argv[1],callback,400);
  return EXIT_SUCCESS;
}
