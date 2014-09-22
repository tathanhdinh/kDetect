#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main(int argc, char* argv[])
{
  char buffer[] = "TA Thanh DinhOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO";
  
  /* duplicate of fd: new_fd0, new_fd1 */
  int fd = open(argv[1], O_CREAT | O_RDWR);
  write(fd, buffer, strlen(buffer));
  
  int new_fd0 = dup(fd); 
  write(new_fd0, buffer, strlen(buffer));
  
  int new_fd1 = dup(new_fd0);
  write(new_fd1, buffer, strlen(buffer));
  
  close(fd);
  close(new_fd0);
  close(new_fd1);
  
  /* duplicate of ffd: new_ffd0 */
  int ffd = open(argv[2], O_APPEND | O_RDWR);
  write(ffd, buffer, strlen(buffer));
  
  close(ffd);
  
  return 0;
}