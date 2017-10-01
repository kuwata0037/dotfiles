DOTPATH    := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CONFPATH   := $(HOME)/.config
CANDIDATES := $(shell find . -maxdepth 1 -mindepth 1 -type d)
EXCLUSIONS := %.git %emacs
DOTFILES   := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

.PHONY: all
all:

## Create symlink of dot files
.PHONY: deploy
deploy: clean
	@echo '==> Start to deploy dotfiles.'
	@$(foreach val, $(DOTFILES), ln -sfnv $(abspath $(val)) $(CONFPATH);)
	@ln -sfnv $(DOTPATH)/emacs $(HOME)/.emacs.d
	@ln -sfnv $(DOTPATH)/zsh/zshenv $(HOME)/.zshenv
	@ln -sfnv $(DOTPATH)/tmux/tmux.conf $(HOME)/.tmux.conf

## Remove the dot files
.PHONY: clean
clean:
	@echo '==> Remove dotfiles.'
	@-$(foreach val, $(DOTFILES), rm -vrf $(CONFPATH)/$(notdir $(val));)
	@-rm -vrf $(HOME)/.emacs.d
	@-rm -vrf $(HOME)/.zshenv
	@-rm -vrf $(HOME)/.tmux.conf
