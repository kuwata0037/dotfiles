########################################
### ALIAS

alias e 'emacsclient -nw -a ""'
alias ekill 'emacsclient -e "(kill-emacs)"'
alias g 'git'
alias fzf 'fzf-tmux'


########################################
### ENVIRONMENT

# Xdg
set -q XDG_CONFIG_HOME
or set -l XDG_CONFIG_HOME ~/.config

# Path
set -l local_paths /usr/local/{sbin,bin} /usr/{sbin,bin} /{sbin,bin}

## go
set -gx GOPATH $HOME/dev
set local_paths $GOPATH/bin $local_paths

## rust
set local_paths ~/.cargo/bin $local_paths

## **env
set local_paths ~/.pyenv/{bin,shims} $local_paths
set local_paths ~/.rbenv/{bin,shims} $local_paths

## update PATH via fish_user_paths
set -g fish_user_paths $local_paths

# Misc
set -gx LANG en_US.UTF-8
set -gx TERM xterm-256color
set -gx GHQ_ROOT $GOPATH/src
set -gx TMUX_PLUGIN_MANAGER_PATH $XDG_CONFIG_HOME/tmux/plugins/


########################################
### PLUGINS

if not functions -q fisher
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end
