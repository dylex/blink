CC=gcc
CFLAGS=-Wall -g -D_GNU_SOURCE=1

all: blink1c blink1d

clean:
	rm -f *.o blink1c blink1d
