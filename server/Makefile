.PHONY: all build run test

all: build run

build:
	stack build

run:
	stack --verbosity error exec Backend-exe 2 10

test:
	stack test
