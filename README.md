# About
My `emacs` configuration.

# Motivation
1. Make emacs fits my way of working.
2. Be really effective at coding and writing.
3. Share my emacs configuration.

# Install

To install `emacs` (snapshot), `cask` and all the dependencies of my
emacs configuration, run the command:

	make install

# Install Emacs

They are several ways to install Emacs on Linux (I'm running Ubuntu
18.04 LTS).

1. To install the latest version of Emacs available on the *official* ubuntu
apt-source packages, run the command:

		sudo apt install emacs

	Note: When Emacs has a new version, Ubuntu Linux typically takes half
	a year to a year to have a prepared package.

2. To install the latest version of Emacs available on ubuntu, run the commands:

		sudo add-apt-repository ppa:ubuntu-elisp/ppa
		sudo apt-get update
		sudo apt install emacs-snapshot

	See [updating emacs from 24 to 26/27](http://iboyko.com/articles/updating-emacs-from-24-to-26-27-on-ubuntu/)
	to get more information.

3. You can also build Emacs from source and install it. See section below.

# Build Emacs from source

Note that I'm running Ubuntu 18.04 LTS.

GNU Emacs source code and development is hosted on
[savannah.gnu.org](http://savannah.gnu.org/projects/emacs/).

You can build emacs from a `release` or from a `repository checkout`.

In both cases, The files [INSTALL](http://git.savannah.gnu.org/cgit/emacs.git/tree/INSTALL)
and [INSTALL.REPO](http://git.savannah.gnu.org/cgit/emacs.git/tree/INSTALL.REPO)
are worth reading. They contains information on building Emacs.

## Additional tools and libraries

In both cases, you need some additional tools and libraries that are maybe
not yet install on your system. With the minimal installation of Ubuntu
18.04 LTS, I just had to install the package `autoconf`, `texinfo`,
`libgtk-3-dev`, `libxpm-dev`, `libjpeg9-dev`, `libgif-dev`, `libtiff-dev`
and `libghc-gnutls-dev`. To install these package, run the command:

	sudo apt install autoconf texinfo libgtk-3-dev \
	libxpm-dev libjpeg9-dev libgif-dev libtiff-dev \
	libghc-gnutls-dev

Note: When installing these packages, I've got some trouble with the
dependency `libjpeg-turbo8-dev` and I fixed it by running the command:

	sudo dpkg -i --force-overwrite \
	/var/cache/apt/archives/libjpeg-turbo8-dev_1.5.2-0ubuntu5.18.04.3_amd64.deb

See [error processing archive](https://askubuntu.com/questions/1026739/dpkg-error-processing-archive-var-cache-apt-archives-cuda-cublas-9-1-9-1-85-3)
to get more information.

## Build from release

Browse the [Emacs releases](https://ftp.gnu.org/gnu/emacs/) and choose the
one you want to install. Here, we build and install the version 26.3 of
Emacs.

1. To build Emacs, download the emacs release, run the script `./configure`
and use `make`. To do so, run the following commands:

		wget https://ftp.gnu.org/gnu/emacs/emacs-26.3.tar.xz
		tar -xf emacs-26.3.tar.xz
		cd emacs-26.3
		./configure
		make

	If you are in the top-level Emacs source directory:

	Now you can run `emacs` this way:

		./src/emacs

	And you can get the emacs version by running the command:

		./src/emacs --version

2. To install Emacs, in the top-level Emacs source directory, run the command:

		sudo make install

3. To uninstall Emacs, in the top-level Emacs source directory, run the command:

		sudo make uninstall

	Note: If you've installed `emacs` by running `make install`, you can't
	remove `emacs` neither with `apt` nor `dpkg`. Specifically, neither commands
	`sudo apt remove emacs` and `sudo dpkg -r emacs` don't work.

4. If you've installed Emacs successfully, you no longer need the Emacs source directory
to use `emacs`.

## Build from repository checkout

1. To build Emacs, clone the `emacs` repository and use the command `make`.
To do so, run the following commands:

		git clone git://git.sv.gnu.org/emacs.git
		cd emacs
		make

	If you are in the top-level Emacs source directory:

	Now you can run `emacs` this way:

		./src/emacs

	And you can get the emacs version by running the command:

		./src/emacs --version

2. To install Emacs, in the top-level Emacs source directory, run the command:

		sudo make install

3. To uninstall Emacs, in the top-level Emacs source directory, run the command:

		sudo make uninstall

	Note: If you've installed `emacs` by running `make install`, you can't
	remove `emacs` neither with `apt` nor `dpkg`. Specifically, neither commands
	`sudo apt remove emacs` and `sudo dpkg -r emacs` don't work.

4. If you've installed Emacs successfully, you no longer need the Emacs source directory
to use `emacs`.

# Cask

[Cask](https://github.com/cask/cask)  is a project management tool for
Emacs that helps automate the package development cycle; development,
dependencies, testing, building, packaging and more.

Cask can also be used to manage dependencies for your local Emacs
configuration.

## Install

To install `cask`, run the command:

	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

Note: `cask` is written both in `emacs-lisp` and `python`, so be sure
to have python (python 2) and emacs installed before installing
`cask`.

Add `cask` to your `$PATH` by putting this following command into
your `~/.bashrc` file:

	export PATH="$HOME/.cask/bin:$PATH"

## Usage

Into your project root, you can produce a `Cask` file containing
boilerplate code by running the command:

	cask init.

Then fill the `Cask` file with the packages your project depends
on. Finally, to install all dependencies, run the command:

	cask install

This will create a `.cask` directory and install all the dependencies
into it.

If you are using `cask` for your emacs configuration, add this to your
`~/.emacs.d/init.el` file:

	(require 'cask "~/.cask/cask.el")
	(cask-initialize)

To show the help about `cask`, run the command `cask help`.

# Tests

To run the `tests` perform with `ert`, run the command:

	make test

To test the whole emacs setting, run the command:

	make test_all

Install the pre-commit hook, so that the tests are always running:

    cp pre-commit .git/hooks/pre-commit


# My Emacs Setup History

If you're interested in the path I've taken to get to the `emacs`
setup I'm currently using, check [My Emacs Setup History](./setup-history.org)
`org` document.

# License

Project under MIT license

**Good setting for clean work. Have a better life.**
