TESTFILES := $(wildcard *.holeyc)
TESTS := $(TESTFILES:.holeyc=.test)

.PHONY: all

all: $(TESTS)

%.test:
	@echo "Testing $*.holeyc"
	@touch $*.out #Creates out file to diff in case of no output
	@touch $*.err #The @ means don't show the command being invoked
	@touch $*.out.expected
	@touch $*.err.expected
	@../holeycc $*.holeyc -n $*.out 2> $*.err ;\
	PROG_EXIT_CODE=$$?;\
	echo "diff output...";\
	diff -B --ignore-all-space $*.out $*.out.expected;\
	OUT_EXIT_CODE=$$?;\
	echo "diff error...";\
	diff -B --ignore-all-space $*.err $*.err.expected;\
	ERR_EXIT_CODE=$$?;\
	exit $$OUT_EXIT_CODE && $$ERR_EXIT_CODE

clean:
	rm *.out *.err
