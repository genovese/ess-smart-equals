CASK ?= cask
EMACS ?= emacs

all: test

test: unit integration

unit:
	${CASK} exec ert-runner

integration:
	${CASK} exec ecukes

install-deps:
	${CASK} install

.PHONY:	all test unit integration install-deps

