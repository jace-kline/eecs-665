TESTFILES := $(wildcard *.holeyc)
TESTS := $(TESTFILES:.holeyc=.test)

.PHONY: all

all: $(TESTS)

%.test:
	@echo "Testing $*.holeyc"
	@touch $*.out #Creates out file to diff in case of no output
	@touch $*.out.expected
	@touch $*.err #The @ means don't show the command being invoked
	@touch $*.err.expected
	@../holeycc $*.holeyc -c 2> $*.err ;\
	PROG_EXIT_CODE=$$?;\
	echo "diff error...";\
	diff -B --ignore-all-space $*.err $*.err.expected;\
	ERR_EXIT_CODE=$$?;\
	exit 0

clean:
	rm *.out *.err
