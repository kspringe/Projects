#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <stdint.h>

int main(int argc, char **argv)
{
    int fdin;
    int fdout = STDOUT_FILENO;
    for(int i = 1; i < argc; i++)
    {
        if (argc == 1)
        {
            fdin = STDIN_FILENO;
        }
        else
        {
            fdin = open(argv[i], O_RDONLY);
        }

        ssize_t bytesRead=512;
	while (bytesRead == 512)
        {
            size_t bufSize=512;
            uint8_t buf[512];
            bytesRead = read(fdin, buf, bufSize);
    
            if (bytesRead <= 0)
            {
                perror(argv[0]);
                exit(errno);
   	    } 
           
            ssize_t bytesWritten;
	    bytesWritten = write (fdout , buf , bytesRead);

            if (bytesWritten != bytesRead)
            {
                perror(argv[0]);
                exit(errno);
            }
        }
        int closein;
        closein = close(fdin);

        if (closein == -1)
        {
            perror(argv[0]);
            exit(errno);
        }

    }

    int closeout;
    closeout = close(fdout);

    if (closeout == -1)
    {
        perror(argv[0]);
        exit(errno);
    }

    return 0;
}
