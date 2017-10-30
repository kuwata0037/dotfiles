DOTPATH    := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CONFPATH   := $(HOME)/.config
CANDIDATES := $(shell find . -maxdepth 1 -mindepth 1 -type d)
EXCLUSIONS := %.git %emacs
DOTFILES   := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

.PHONY: all
all: help

.PHONY: deploy
deploy: clean ## Create symlink of dot files
	@echo '==> Start to deploy dotfiles.'
	@$(foreach val, $(DOTFILES), ln -sfnv $(abspath $(val)) $(CONFPATH);)
	@ln -sfnv $(DOTPATH)/emacs $(HOME)/.emacs.d
	@ln -sfnv $(DOTPATH)/zsh/zshenv $(HOME)/.zshenv
	@ln -sfnv $(DOTPATH)/tmux/tmux.conf $(HOME)/.tmux.conf

.PHONY: clean
clean: ## Remove the dot files
	@echo '==> Remove dotfiles.'
	@-$(foreach val, $(DOTFILES), rm -vrf $(CONFPATH)/$(notdir $(val));)
	@-rm -vrf $(HOME)/.emacs.d
	@-rm -vrf $(HOME)/.zshenv
	@-rm -vrf $(HOME)/.tmux.conf

.PHONY: help
help: ## Show self-documented Makefile
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
