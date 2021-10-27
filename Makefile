DOTPATH    := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CONFPATH   := $(HOME)/.config
CANDIDATES := $(shell find . -maxdepth 1 -mindepth 1 -type d)
EXCLUSIONS := %.git %emacs
DOTFILES   := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

.PHONY: all
all: help

.PHONY: deploy
deploy: clean ## Create symlink of dotfiles to $HOME
	@echo '==> Start to deploy dotfiles.'
	@$(foreach val, $(DOTFILES), ln -sfnv $(abspath $(val)) $(CONFPATH);)
	@ln -sfnv $(DOTPATH)/emacs $(HOME)/.emacs.d
	@ln -sfnv $(DOTPATH)/zsh/zshenv $(HOME)/.zshenv

.PHONY: clean
clean: ## Remove the dotfiles from $HOME
	@echo '==> Remove dotfiles.'
	@-$(foreach val, $(DOTFILES), rm -vrf $(CONFPATH)/$(notdir $(val));)
	@-rm -vrf $(HOME)/.emacs.d
	@-rm -vrf $(HOME)/.zshenv

.PHONY: ignore
ignore: ## Update git ignore file
	@echo '==> Update git ignore.'
	@gibo update
	@gibo dump Linux macOS Emacs Vim JetBrains VisualStudioCode > $(DOTPATH)/git/ignore

.PHONY: completion
completion: ## Update fish completion files
	@echo '==> Update fish completion.'
	@-rustup completions fish > $(DOTPATH)/fish/completions/rustup.fish

.PHONY: help
help: ## Show self-documented Makefile
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "...\033[36m%-10s\033[0m %s\n", $$1, $$2}'
