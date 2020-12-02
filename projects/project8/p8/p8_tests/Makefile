HOLEYC_FILES := $(wildcard *.holeyc)
TESTS := $(HOLEYC_FILES:.holeyc=.test)

.PHONY: all clean

all: $(TESTS)

%.test:
	@printf "TEST %s:\n" "$*" ;\
	../holeycc $*.holeyc -a $*.3ac.original ;\
	../holeycc $*.holeyc -z -a $*.3ac ;\
	diff -B --ignore-all-space $*.3ac $*.3ac.expected ;\
	exit 0

clean:
	rm -rf *_cfgs *.3ac *.3ac.original