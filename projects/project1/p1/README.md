# EECS 665 Project 1
### Author: Jace Kline

## Description
In this project, we implement a lexer for the programming language called HolyC, a compact language with C-style semantics. We use the tool Flex to generate a C++ lexer class which we then use to process and tokenize raw string input from .holyc files into a list of tokens.

## Testbench Usage
- Each predefined test resides in a directory stored within the 'tests' directory
- For each test, there shall be, at a minimum, the following files: test.holyc, test.out.expected, and test.err.expected
- To run a test Makefile target, use the command `make <target>` where the test target options are listed below
- There are four Makefile test targets of interest:
1. *test-jace*: 
   - Runs the tests and outputs a diff-out.txt and diff-err.txt in each test folder
   - additionally, combines diff outputs to the file 'tests/diffs.txt' for view.
2. *test*:
   - Tests must be in the main project directory and labeled as test#.holyc with corresponding test#.out.expected and test#.err.expected files present
   - (If desired) Must run `./copy-tests.sh` before to load the tests from the 'tests' directory into the main project directory
   - Running this target outputs the diff outputs to stdout for each test present in the directory
3. *cleantests*:
   - for each test #, removes the test#.out and test#.err files
4. *removetests*:
   - removes all test files from the main project directory
