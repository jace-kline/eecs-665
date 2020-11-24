GHC_FILES := $(wildcard *.hs)

.PHONY: clean

all: dragoninterp

dragoninterp: $(GHC_FILES)
	ghc Main.hs -o holeycc

clean:
	rm -rf *.hi *.o holeycc

