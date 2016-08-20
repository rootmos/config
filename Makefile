APT=sudo apt-get
INSTALL=$(APT) install
GIT_ROOT=$(HOME)/git
CONFIG_ROOT=$(GIT_ROOT)/config
CONFIG_REPO=https://github.com/rootmos/config
PACKAGES=cmake python-dev

all: build-essentials config vim

$(PACKAGES):
	$(INSTALL) $@

## Build essentials
##############################################################################
.PHONY: build-essentials
build-essentials:
	$(INSTALL) autoconf automake autotools-dev binutils build-essential cpp g++ gcc make

## Git
##############################################################################
git:
	$(INSTALL) git
	git config --global core.editor vim
	git config merge.tool vimdiff

## Config
##############################################################################
$(CONFIG_ROOT):
	mkdir -p $(dir $@)
	git clone $(CONFIG_REPO) $(CONFIG_ROOT)

config: $(CONFIG_ROOT)
	(cd $(CONFIG_ROOT) && git pull)

## Vim
##############################################################################
VIMRC=$(HOME)/.vimrc
VIM_DIR=$(HOME)/.vim

$(VIM_DIR):
	mkdir -p $@

$(VIMRC): $(CONFIG_ROOT)
	test -f $@ || ln -s $(CONFIG_ROOT)/.vimrc $@

BUNDLE=$(VIM_DIR)/bundle
VUNDLE=$(BUNDLE)/Vundle.vim
VUNDLE_REPO=https://github.com/VundleVim/Vundle.vim.git
$(VUNDLE): $(VIM_DIR)
	mkdir -p $(dir VUNDLE)
	git clone $(VUNDLE_REPO) $(VUNDLE)

vundle: $(VUNDLE) $(VIMRC)
	(cd $(VUNDLE) && git pull)
	vim -c "BundleInstall"


YOUCOMPLETEME=$(BUNDLE)/YouCompleteMe
$(YOUCOMPLETEME)/ycm_global_conf.py: vundle config
	test -f $@ || ln -s $(CONFIG_ROOT)/.vim/ycm_global_conf.py $@
YouCompleteMe: vundle cmake python-dev $(YOUCOMPLETEME)/ycm_global_conf.py
	(cd $(YOUCOMPLETEME) && ./install.py)

vim: vim-install $(VIMRC) vundle YouCompleteMe

vim-install:
	$(INSTALL) vim

## XMonad
##############################################################################

xmonad-install:
	$(INSTALL) xmonad conky dzen2 suckless-tools

xmonad: xmonad-install
	ln -s $(CONFIG_ROOT)/.xmonad $(HOME)/.xmonad

## urxvt
##############################################################################

urxvt: Xdefaults
	$(INSTALL) rxvt-unicode-256color

## Xdefaults
##############################################################################

Xdefaults:
	ln -s $(CONFIG_ROOT)/.Xdefaults $(HOME)/.Xdefaults


## tmux
##############################################################################

tmux:
	$(INSTALL) tmux
	ln -s $(CONFIG_ROOT)/.tmux.conf $(HOME)/.tmux.conf

## aliases 
##############################################################################

aliases:
	ln -s $(CONFIG_ROOT)/.bash_aliases $(HOME)/.bash_aliases
