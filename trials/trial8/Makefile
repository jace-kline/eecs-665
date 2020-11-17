GHC_FILES := $(wildcard *.hs)

.PHONY: clean

all: dragoninterp

dragoninterp: $(GHC_FILES)
	ghc Main.hs -o dragoninterp

clean:
	rm -rf *.hi *.o dragoninterp

