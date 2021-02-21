# -*- Makefile -*-

TEXI_CHAPTER := EditorConfig Emacs Plugin

EMACS = emacs
PANDOC = pandoc
AWK = awk

PROJECT_ROOT_DIR = $(CURDIR)
ERT_TESTS = $(wildcard $(PROJECT_ROOT_DIR)/ert-tests/*.el)

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -q --no-site-file -L $(PROJECT_ROOT_DIR)

MAIN_SRC = editorconfig.el
SRCS = $(wildcard $(PROJECT_ROOT_DIR)/*.el)
OBJS = $(SRCS:.el=.elc)

.PHONY: check \
	compile clean \
	test test-ert test-core \
	sandbox doc

check: compile test


compile: $(OBJS)

$(OBJS): %.elc: %.el
	$(EMACS) $(BATCHFLAGS) -f batch-byte-compile $^

clean:
	-rm -f $(OBJS)


doc: doc/editorconfig.texi

doc/editorconfig.texi: README.md doc/header.txt
	mkdir -p doc
	tail -n +4 $< | $(PANDOC) -s -f markdown -t texinfo -o $@.body
	$(AWK) 'f{print} /^@chapter $(TEXI_CHAPTER)/{f=1;print}' $@.body >$@.body2
	cat doc/header.txt $@.body2 >$@
	rm -f $@.body $@.body2

test: test-ert test-core
	$(EMACS) $(BATCHFLAGS) -l editorconfig.el


# ert test
test-ert: $(ERT_TESTS) $(OBJS)
	$(EMACS) $(BATCHFLAGS) \
		--eval "(setq debug-on-error t)" \
		--eval "(require 'ert)" \
		--eval "(setq metadata-el-files '($(MAIN_SRC:%=\"%\")))" \
		$(ERT_TESTS:%=-l "%") \
		-f ert-run-tests-batch-and-exit



# Core test
core-test/CMakeLists.txt:
	git submodule init

test-core: core-test/CMakeLists.txt $(OBJS)
	cd $(PROJECT_ROOT_DIR)/core-test && \
		cmake -DEDITORCONFIG_CMD="$(PROJECT_ROOT_DIR)/bin/editorconfig-el" .
	cd $(PROJECT_ROOT_DIR)/core-test && \
		EMACS_BIN=$(EMACS) EDITORCONFIG_CORE_LIBRARY_PATH="$(PROJECT_ROOT_DIR)" \
		ctest --output-on-failure .


# Start Emacs that loads *.el in current directory and does not load the user
# init file
sandbox:
	$(EMACS) -q -L $(PROJECT_ROOT_DIR) $(MAIN_SRC:%=-l "%")
