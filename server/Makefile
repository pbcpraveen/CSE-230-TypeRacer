.PHONY: all build run test

all: build run

build:
	stack build

run:
	stack --verbosity error exec Backend-exe 4 4

test:
	stack test
