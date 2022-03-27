DOTPATH    := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))

.PHONY: all
all: help

.PHONY: ignore
ignore: ## Update git ignore file
	@echo '==> Update git ignore.'
	@gibo update
	@gibo dump Linux macOS Emacs Vim JetBrains VisualStudioCode > $(DOTPATH)/private_dot_config/private_git/ignore

.PHONY: completion
completion: ## Update fish completion files
	@echo '==> Update fish completion.'
	@-rustup completions fish > $(DOTPATH)/private_dot_config/private_fish/completions/rustup.fish

.PHONY: help
help: ## Show self-documented Makefile
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "...\033[36m%-10s\033[0m %s\n", $$1, $$2}'
