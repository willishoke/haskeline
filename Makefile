.PHONY: go
go: build
	stack exec brickline

.PHONY: build
build:
	stack build
