TEST_FILES := $(wildcard test*.holyc)
TESTS := $(TEST_FILES:.holyc=)
TEST_CLEANS := $(addsuffix .clean,$(TESTS))
# TESTS := $(TEST_SRCS:.holyc=.err)

.PHONY: test clean

test: $(TESTS)

test%: test%.holyc
	@printf "%s\n" "Running $@: "
	@touch $@.tokens
	@../holycc $< -t $@.tokens 1> /dev/null
	@../holycc $< -u $@.unparsed 1> /dev/null 2> $@.err
	@-if test -f "@.unparsed"; then diff $@.unparsed $@.tokens; fi
	@echo "Token diff:"
	@echo "Error diff:"
	@touch $@.err.expected
	@-diff $@.err $@.err.expected
	@printf "%s\n\n" "$@ complete"

clean: $(TEST_CLEANS)
	@echo "Cleaning up test directories..."

%.clean: %.holyc
	@rm -rf $*.err $*.unparsed $*.tokens
