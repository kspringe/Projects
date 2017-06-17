CC = gcc
CFLAGS = -g -Wall -Wextra -Wpedantic -Werror -std=c99
OBJECTS =

all: increment

increment: increment.c
	$(CC) $(CFLAGS) -o increment increment.c -lpthread

clean:
	rm $(OBJECTS) increment
