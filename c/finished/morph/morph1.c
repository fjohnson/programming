#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

/*
  Friday Apr 22: 2011.
  Purpose: Generate a modified version of the code you see here, run it,
           and repeat.

  Details:
  Check if this file is the 8th compile as a terminating condition. We can tell
  this by the filename 'morph 8'.
  Otherwise read in this entire file, write out this file again but change the
  output to print the current iteration. Compile the program and change core.
  In the end we should have 8 source files morph{1-8}
*/

int main(int argc, char *argv[]){

  /*Print current iteration and stop if the 8th*/
  size_t pathlen = strlen(argv[0]);
  char *name = basename(argv[0]);
  char source[pathlen + 2 + 1]; /*Location of source code for this file*/
  char *newsource; /*Next location for modified source code*/
  
  memcpy(source,argv[0],pathlen*sizeof(char));
  memcpy(source+pathlen,".c",3*sizeof(char));

  newsource = strdupa(source);
  newsource[pathlen-1]++;

  printf("%s ITERATION:1\n",name); /*line to modify*/
  if(strstr(name,"morph8")){ return EXIT_SUCCESS; } /*terminating condition*/

  /*Open source for modification*/

  FILE *sourcefile = fopen(source,"r");
  FILE *newsourcefile = fopen(newsource,"w");
  if(!source || !newsourcefile){
    fprintf(stderr,"Error opening or creating new source. Aborting %s\n",name);
    return fcloseall();
  }

  char *linebuf = NULL;
  size_t lbufsize = 0; 
  ssize_t bytesread = 0;
  char *modpoint = NULL;
  
  while( (bytesread = getline(&linebuf, &lbufsize, sourcefile)) != -1 ){
      
      modpoint = strstr(linebuf,"ITERATION:");
      
      if(modpoint){
	if(!strstr(linebuf,"strstr")){ /* dont match the replacement line! */
	  modpoint[10]++;
	}
      }
      
      if(fputs(linebuf,newsourcefile) == EOF){
	fprintf(stderr,"Error writing to new source file. Aborting %s\n",name);
	fcloseall();
	return EXIT_FAILURE;
      }
  }

  if(linebuf) free(linebuf);
  if(fclose(sourcefile) == EOF){
    perror("Failed to close source file");
    return EXIT_FAILURE;
  }
  if(fclose(newsourcefile) == EOF){
    perror("Failed to close new source file");
    return EXIT_FAILURE;
  }
  
  /*gcc -o ~/c/morph ~/c/morph.c*/

  size_t slen = strlen(newsource);
  char exename[slen-2];
  memcpy(exename,newsource,(slen-2)*sizeof(char));
  exename[slen-2] = 0;
  
  pid_t fr = fork();
  int childstatus;
  if(fr == -1) perror(0);
  else if(!fr){
  
    char *const argv[] = {"/usr/bin/gcc", "-o",exename, newsource, 0};
    execv("/usr/bin/gcc",argv);
    perror("Execv returned");
    _exit(EXIT_FAILURE);

  }
  fr = wait(&childstatus);
  if(fr == -1) perror(0);

  /*Execute the next iteration*/
  char *lineptr = NULL;
  size_t lengthofline = 0;
  ssize_t readresult = 0;
  while( readresult = getline(&lineptr,&lengthofline,stdin) != -1){
    puts("Ctrl+D to proceed to next iteration\n");
  }
  if(lineptr) free(lineptr);
  else perror("User input failed");
  
  char *const arg[] = {exename,0};
  execv(exename,arg);
  perror("Execv returned after a successful compile"); 
  return EXIT_FAILURE;
}

