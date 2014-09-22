#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

int main(int argc, char* argv[])
{
  char buffer[] = "TA Thanh Dinh";
  
  int fd0 = open(argv[1], O_RDWR);
  write(fd0, buffer, strlen(buffer) + 1);
  
  int fd1 = dup(fd0);
  write(fd1, buffer, strlen(buffer) + 1);
  
  close(fd0); close(fd1);
  
  int fd2 = open(argv[2], O_RDWR);
  write(fd2, buffer, strlen(buffer) + 1);
  
  close(fd2);
  
  return 0;
}