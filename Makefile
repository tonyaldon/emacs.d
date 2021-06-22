emacs ?= emacs

.PHONY: all install_emacs uninstall_emacs \
        install_cask uninstall_cask install_cask_dependencies \
        install

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
	fi ; \
	export PATH="$$HOME/.cask/bin:$$PATH"

uninstall_cask:
	@rm -rf $$HOME/.cask

install_cask_dependencies:
	@cask install

install: install_emacs install_cask install_cask_dependencies
