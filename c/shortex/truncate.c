#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>

/*Truncate the first argument to size 0*/

int main(int argc, char **argv){
  int result;

  if(argc > 1){
    result = truncate(argv[1], 0);
    
    if(result == -1){
      perror("Truncate failed");
      return 1;
    }
  }
  return 0;
}
