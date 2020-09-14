TEST_FILES := $(wildcard test*.holyc)
TESTS := $(TEST_FILES:.holyc=)
TEST_CLEANS := $(addsuffix .clean,$(TESTS))
# TESTS := $(TEST_SRCS:.holyc=.err)

.PHONY: test clean

test: $(TESTS)

test%: test%.holyc
	@printf "%s\n" "Running $@: "
	@../holycc $< -t $@.tokens
	@../holycc $< -p 1> $@.unparsed 2> $@.err
	-diff $@.unparsed $@.tokens
	@touch $@.err.expected
	-diff $@.err $@.err.expected
	@printf "%s\n\n" "$@ complete"

clean: $(TEST_CLEANS)
	@echo "Cleaning up test directories..."

%.clean: %.holyc
	@rm -rf $*.err $*.unparsed $*.tokens
