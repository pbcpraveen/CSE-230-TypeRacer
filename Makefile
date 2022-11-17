.PHONY: all build run

all: build run

build:
	stack build --ghc-options="-threaded"

run:
	stack --verbosity error exec TypeRacer-exe
