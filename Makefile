emacs ?= emacs
LOAD_ALL = -l init.el
LOAD_ELPA = -l elpa.el
LOADTEST = $(foreach file,$(wildcard test/settings/*),-l $(file))
LOAD = $(LOADTEST:test/settings/%-test.el=settings/%.el)

.PHONY: all test test_all install_emacs uninstall_emacs \
        install_cask uninstall_cask install_cask_dependencies \
        install

all: test_all

test_all:
	@echo "Using $(shell which $(emacs))..."
	$(emacs) -batch $(LOAD_ALL) $(LOADTEST) \
	-f ert-run-tests-batch-and-exit

test:
	@echo "Using $(shell which $(emacs))..."
	$(emacs) -batch $(LOAD_ELPA) $(LOAD) $(LOADTEST) \
	-f ert-run-tests-batch-and-exit

install_emacs:
	@sudo add-apt-repository ppa:ubuntu-elisp/ppa ; \
	sudo apt-get update ; \
	sudo apt install emacs-snapshot

uninstall_emacs:
	@sudo apt remove emacs-snapshot ; \
	sudo rm /etc/apt/sources.list.d/ubuntu-elisp-ubuntu-ppa-bionic.list*

install_cask:
	@curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python ; \
	if [ -z "$$(grep .cask $$HOME/.bashrc)" ]; then \
	  printf '\nexport PATH="$$HOME/.cask/bin:$$PATH"' >> $$HOME/.bashrc ; \
	fi

uninstall_cask:
	@rm -rf $$HOME/.cask

install_cask_dependencies:
	@cask install

install: install_emacs install_cask install_cask_dependencies
