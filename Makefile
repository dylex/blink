LOCAL_CC=gcc
LOCAL_CFLAGS=-Wall -g

DEST_MOD=/lib/modules/misc
DEST_BIN=/usr/bin
DEST_SBIN=/usr/sbin

all: ppldc pplddd ppldd.ko

#ppldd.o: ppldd.c /usr/include/linux/version.h
#	$(CC) $(MODCFLAGS) -c $<

%: %.o
	$(CC) -o $@ $<

%.o: %.c ppldd.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o ppldc pplddd

install: $(DEST_BIN)/ppldc $(DEST_SBIN)/pplddd ppldd.ko
	make -C /usr/src/linux SUBDIRS=$(PWD) modules_install
#	install ppldc /usr/bin/ppldc
#	install pplddd /usr/sbin/pplddd
#	install ppldd.o /lib/modules/misc/ppldd.o

#$(DEST_MOD)/ppldd.o: ppldd.o
#	install $< $@

$(DEST_BIN)/ppldc: ppldc
	install $< $@

$(DEST_SBIN)/pplddd: pplddd
	install $< $@

ppldd.ko: ppldd.c
	make -C /usr/src/linux SUBDIRS=$(PWD) modules

obj-m += ppldd.o
