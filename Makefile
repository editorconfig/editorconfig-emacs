# -*- Makefile -*-

EMACS = emacs

PROJECT_ROOT_DIR = $(PWD)
ERT_TESTS = $(wildcard $(PROJECT_ROOT_DIR)/ert-tests/*.el)
TRAVIS_FILE = .travis.yml

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -q --no-site-file -L $(PROJECT_ROOT_DIR)

MAIN_SRC = editorconfig.el
SRCS = editorconfig.el editorconfig-core.el editorconfig-core-handle.el \
	editorconfig-fnmatch.el
OBJS = $(SRCS:.el=.elc)

$(OBJS): %.elc: %.el
	$(EMACS) $(BATCHFLAGS) -f batch-byte-compile $^

.PHONY: all clean test test-travis test-ert test-core test-metadata

all: $(OBJS)

clean:
	-rm -f $(OBJS)

test: test-ert test-core test-metadata $(OBJS)
	$(EMACS) $(BATCHFLAGS) -l editorconfig.el

test-travis:
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi



# ert test
test-ert: $(ERT_TESTS) $(OBJS)
	$(EMACS) $(BATCHFLAGS) \
		--eval "(require 'ert) (setq debug-on-error t)" \
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
