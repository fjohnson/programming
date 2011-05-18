/*
  Here is some code testing out the Inotify api provided by the linux kernel.
  The date for this is sometime between March and May of 2011.
  Author: Fletcher Johnson
  Email: flt.johnson@gmail.com
*/
#define _GNU_SOURCE
/*#define NDEBUG*/
#define message(message,name) fprintf(stdout,message,name)

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/inotify.h>
#include <assert.h>
#include <errno.h>
#include <time.h>
#include <string.h>

char * get_time(void){
  time_t tloc;
  struct tm * timestruct;

  time(&tloc);
  timestruct = localtime(&tloc);
  return asctime(timestruct);
}

void describe_event(struct inotify_event *e){
  char *time = get_time();
  size_t tmsglen = strlen(time);
  
  char timeprefix[tmsglen];
  strncpy(timeprefix,time,tmsglen-1); /*shave off trailing newline*/
  timeprefix[tmsglen-1] = 0;
  printf("%s - ",timeprefix);

  switch(e->mask){
  case IN_ACCESS:
    message("File was accessed: %s\n",e->name);
    break;
  case IN_MODIFY:
    message("File was modified: %s\n",e->name);
    break;
  case IN_ATTRIB:
    message("Metadata change: %s\n",e->name);
    break;
  case IN_CLOSE_WRITE:
    message("Writtable file was closed: %s\n",e->name);
    break;
  case IN_CLOSE_NOWRITE:
    message("Unwrittable file was closed: %s\n",e->name);
    break;
  case IN_CLOSE:
    message("File closed: %s\n",e->name);
    break;
  case IN_OPEN:
    message("File was opened: %s\n",e->name);
    break;
  case IN_MOVED_FROM:
    message("File was moved from: %s\n",e->name);
    break;
  case IN_MOVED_TO:
    message("File was moved to: %s\n",e->name);
    break;
  case IN_MOVE:
    message("Moves: %s\n",e->name);
    break;
  case IN_CREATE:
    message("Subfile was created: %s\n",e->name);
    break;
  case IN_DELETE:
    message("Subfile was deleted: %s\n",e->name);
    break;
  case IN_DELETE_SELF:
    printf("Self was deleted\n");
    break;
  case IN_MOVE_SELF:
    printf("Self was moved\n");
    break;

  /* Kernel events */
  case IN_UNMOUNT:
    printf("Backing fs was unmounted\n");
    break;
  case IN_Q_OVERFLOW:
    printf("Event queue overflowed\n");
    break;
  case IN_IGNORED:
    message("File was ignored: %s\n",e->name);
    break;
  
  default :
    message("Unknown event %x\n",e->mask);
    break;
  }
}

int main(int argc, char *const argv[]){
  int inotify_instance = inotify_init();
  int watchfd;
  ssize_t length,i;
  size_t ibuflen = 1024 + sizeof(struct inotify_event) + 1;
  char instancebuf[ibuflen];
  struct inotify_event *event;

  /* Create a new inotify instance */
  watchfd = inotify_add_watch(inotify_instance, argv[1], IN_ALL_EVENTS);
  if(watchfd == -1){
    assert_perror(errno);
    exit(EXIT_FAILURE);
  }

  /* Read in pending events  */
  while( (length = read(inotify_instance, instancebuf, ibuflen)) > 0){
    
    i = 0;
    while(i != length){
      event = (struct inotify_event *) (instancebuf+i);
      describe_event(event);
      i += sizeof(struct inotify_event) + event->len;
    }
  }

  /* Clean up */
  if( inotify_rm_watch(inotify_instance, watchfd) == -1){
    assert_perror(errno);
    exit(EXIT_FAILURE);
  }

  if(length == -1) assert_perror(errno);
  if( close(watchfd) == -1 ){
    assert_perror(errno);
    exit(EXIT_FAILURE);
  }
  if (close(inotify_instance) == -1){
    assert_perror(errno);
    exit(EXIT_FAILURE);
  }
  return EXIT_SUCCESS;
}


  
