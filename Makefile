# -*- Makefile -*-

EMACS = emacs
PANDOC = pandoc

PROJECT_ROOT_DIR = $(PWD)
ERT_TESTS = $(wildcard $(PROJECT_ROOT_DIR)/ert-tests/*.el)
TRAVIS_FILE = .travis.yml

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -q --no-site-file -L $(PROJECT_ROOT_DIR)

MAIN_SRC = editorconfig.el
SRCS = editorconfig.el editorconfig-core.el editorconfig-core-handle.el \
	editorconfig-fnmatch.el
OBJS = $(SRCS:.el=.elc)

TEXI ?= doc/editorconfig.texi

$(OBJS): %.elc: %.el
	$(EMACS) $(BATCHFLAGS) -f batch-byte-compile $^

.PHONY: all clean test test-travis test-ert test-core test-metadata test-doc-updated sandbox doc info

all: $(OBJS)

clean:
	-rm -f $(OBJS)


doc: $(TEXI)

$(TEXI): README.md doc/header.txt
	mkdir -p doc
	tail -n +4 $< | $(PANDOC) -s -f markdown -t texinfo -o $@.body
	cat doc/header.txt $@.body >$@


test: test-ert test-core test-metadata test-doc-updated $(OBJS)
	$(EMACS) $(BATCHFLAGS) -l editorconfig.el

test-travis:
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi



# ert test
test-ert: $(ERT_TESTS) $(OBJS)
	git submodule update --init
	$(EMACS) $(BATCHFLAGS) \
		--eval "(setq debug-on-error t)" \
		--eval "(require 'ert)" \
		--eval "(setq metadata-el-files '($(MAIN_SRC:%=\"%\")))" \
		$(ERT_TESTS:%=-l "%") \
		-f ert-run-tests-batch-and-exit



# Core test
core-test/CMakeLists.txt:
	git submodule update --init

test-core: core-test/CMakeLists.txt $(OBJS)
	cd $(PROJECT_ROOT_DIR)/core-test && \
		cmake -DEDITORCONFIG_CMD="$(PROJECT_ROOT_DIR)/bin/editorconfig-el" .
	cd $(PROJECT_ROOT_DIR)/core-test && \
		EMACS_BIN=$(EMACS) EDITORCONFIG_CORE_LIBRARY_PATH="$(PROJECT_ROOT_DIR)" \
		ctest --output-on-failure .


test-doc-updated:
	make doc TEXI=$(TEXI).tmp
	cmp $(TEXI) $(TEXI).tmp
	rm -f $(TEXI).tmp




# Start Emacs that loads *.el in current directory and does not load the user
# init file
sandbox:
	$(EMACS) -q -L $(PROJECT_ROOT_DIR) $(MAIN_SRC:%=-l "%")
