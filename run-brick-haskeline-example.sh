#!/usr/bin/make -f

.PHONY: example
example: build
	stack exec -- /bin/sh -c "cd examples; runhaskell brick-haskeline.hs"

.PHONY: build
build:
	stack build --flag haskeline:brick
