LOCAL_CC=gcc
LOCAL_CFLAGS=-Wall -g -D_GNU_SOURCE=1

all: ppldc pplddd ppldd.ko

#ppldd.o: ppldd.c /usr/include/linux/version.h
#	$(CC) $(MODCFLAGS) -c $<

pplddd: pplddd.o
ppldc: ppldc.o

%.o: %.c ppldd.h
	$(LOCAL_CC) $(LOCAL_CFLAGS) -c $<

%: %.o
	$(LOCAL_CC) $(LOCAL_LDFLAGS) -o $@ $<

clean:
	rm -f *.o ppldc pplddd

install: install-ppldc install-pplddd ppldd.ko
	make -C /usr/src/linux SUBDIRS=$(PWD) modules_install
#	install ppldc /usr/bin/ppldc
#	install pplddd /usr/sbin/pplddd
#	install ppldd.o /lib/modules/misc/ppldd.o

install-ppldc: ppldc
	install $< /usr/bin

install-pplddd: pplddd
	install $< /home/dylan/bin

ppldd.ko: ppldd.c /usr/src/linux/include/linux/version.h
	make -C /usr/src/linux SUBDIRS=$(PWD) modules

obj-m += ppldd.o
