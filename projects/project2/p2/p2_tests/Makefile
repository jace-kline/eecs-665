TEST_SRCS := $(wildcard *.holyc)
TESTS := $(TEST_SRCS:.holyc=.err)

all: test

.PHONY: all test clean

test: $(TESTS)
	@echo "Running tests..."

test%.err: test%.holyc
	@printf "%s" "$*: "
	@touch $@
	@../holycc $< -p 2> $@
	@printf "%s\n" "Complete"

clean:
	rm -rf *.err