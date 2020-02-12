emacs ?= emacs
LOAD_ALL = -l init.el
LOAD_ELPA = -l elpa.el
LOADTEST = $(foreach file,$(wildcard test/settings/*),-l $(file))
LOAD = $(LOADTEST:test/settings/%-test.el=settings/%.el)

.PHONY: all test test_all

all: test_all

test_all:
	@echo "Using $(shell which $(emacs))..."
	$(emacs) -batch $(LOAD_ALL) $(LOADTEST) \
	-f ert-run-tests-batch-and-exit

test:
	@echo "Using $(shell which $(emacs))..."
	$(emacs) -batch $(LOAD_ELPA) $(LOAD) $(LOADTEST) \
	-f ert-run-tests-batch-and-exit
