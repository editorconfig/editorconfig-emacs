# -*- Makefile -*-

EMACS = emacs

TEST_DIR = ert-tests
TRAVIS_FILE = .travis.yml

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -q --no-site-file

SRCS = editorconfig.el
OBJS = $(SRCS:.el=.elc)

%.elc: %.el
	${EMACS} $(BATCHFLAGS) -f batch-byte-compile $^

.PHONY: all clean test test-travis

all: $(OBJS)

clean:
	-rm -f $(OBJS)

test:
	${EMACS} $(BATCHFLAGS) -l editorconfig.el

test-travis:
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi
