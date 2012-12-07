CC=gcc
CFLAGS=-Wall -g -D_GNU_SOURCE=1

all: ppldc pplddd

clean:
	rm -f *.o ppldc pplddd
