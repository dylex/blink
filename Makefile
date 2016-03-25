CC=gcc
CFLAGS=-O -Wall -g -D_GNU_SOURCE=1
LDFLAGS=-g -lm
BINDIR=/usr/bin

FILES=blinkd mail loadavg pinger purple command remote activity notify watch blink1
PROGS=blinkd blink blinkhd blinkh

default: $(PROGS)

blinkd: $(addsuffix .o,$(FILES))
blink: blink.o

HSC=Pinger

blinkhd: Pinger.hs

%: %.hs FORCE
	ghc --make -Wall $@

%-prof: %.hs FORCE
	ghc --make -Wall -rtsopts -prof -fprof-auto -o $@ $<

%.hs: %.hsc
	hsc2hs $<

clean:
	rm -f *.o blink blink1d

.depend: $(addsuffix .c,$(FILES)) blink.c
	gcc -MM $^ > $@

install: $(PROGS)
	install -t $(BINDIR) $^

.PHONY: FORCE
include .depend
