CC = gcc
CFLAGS=-Wall -Wextra -Wpedantic -Werror -std=c99
OBJECTS=hello.o cat.o

.PHONY: all
all: cat hello

cat: cat.o
	$(CC) $(CFLAGS) -o cat cat.o

hello: hello.o
	$(CC) $(CFLAGS) -o hello hello.o

hello.o: hello.c
	$(CC) $(CFLAGS) -c hello.c

cat.o: cat.c
	$(CC) $(CFLAGS) -c cat.c

.PHONY: clean
clean:
	rm $(OBJECTS) cat hello
