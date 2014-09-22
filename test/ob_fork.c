#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <string.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int main(int argc, char* argv[])
{
  char buffer[] = "TaThanhDinh\n";
  
  int fd0 = open(argv[1], O_CREAT | O_RDWR);
  if (fork() == 0) {                          /* child process */
    write(fd0, buffer, strlen(buffer) + 1);
    int fd1 = dup(fd0);
    read(fd1, buffer, strlen(buffer) + 1);
    close(fd1); close(fd0);
  }
  else {                                       /* parent process */
    int fd1 = open(argv[2], O_CREAT | O_RDWR);
    write(fd1, buffer, strlen(buffer) + 1);
    close(fd1);
    
    wait(NULL);
  }
  
  return 0;
}