TESTFILES := $(wildcard *.holeyc)
TESTS := $(TESTFILES:.holeyc=.test)
ASMS := $(TESTFILES:.holeyc=.link)
LIBLINUX := -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib/x86_64-linux-gnu/crt1.o /usr/lib/x86_64-linux-gnu/crti.o /usr/lib/x86_64-linux-gnu/crtn.o

.PHONY: all

all: $(TESTS)
links: $(ASMS) # Take all .s files and assemble, link them

%.test:
	@rm -f $*.err $*.3ac $*.s
	@touch $*.err $*.3ac $*.s $*.in $*.out.expected
	@echo "TEST $*"
	@../holeycc $*.holeyc -o $*.s ;\
	COMP_EXIT_CODE=$$?;
	@as $*.s -o $*.o
	@ld -o $*.prog $(LIBLINUX) -lc ../stdholeyc.o $*.o
	@objdump -Dwrt $*.o > $*.dump
	@./$*.prog < $*.in > $*.out; \
	diff -B --ignore-all-space $*.out $*.out.expected;\
	RUN_DIFF_EXIT=$$?;\
	exit 0

%.link:
	@as $*.s -o $*.o
	@ld -o $*.prog $(LIBLINUX) -lc ../stdholeyc.o $*.o

clean:
	rm -f *.3ac *.out *.err *.o *.s *.dump
