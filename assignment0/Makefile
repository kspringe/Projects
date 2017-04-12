CFLAGS=-Wall -Wextra -Werror -pedantic
CC=gcc $(CFLAGS)

lrc: lrc.o
	$(CC) -o lrc lrc.o

lrc.o: lrc.c
	$(CC) -c lrc.c

clean:
	rm -f lrc lrc.o

