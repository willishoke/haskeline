build:
	stack build haskeline:haskeline-brick

run: build
	stack exec haskeline-brick

.PHONY: build run
