########## env ##########

##### locale #####
set -gx LANG en_US.UTF-8

##### term #####
set -gx TERM xterm-256color

##### path #####
set -l local_paths /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

# go
set -gx GOPATH $HOME/dev
set local_paths $GOPATH/bin $local_paths

# rust
set local_paths ~/.cargo/bin $local_paths

# pyenv
set local_paths ~/.pyenv/bin $local_paths
set local_paths ~/.pyenv/shims $local_paths

# rbenv
set local_paths ~/.rbenv/bin $local_paths
set local_paths ~/.rbenv/shims $local_paths

# update PATH via fish_user_paths
set -g fish_user_paths $local_paths

##### misc #####
set -gx GHQ_ROOT $GOPATH/src
set -gx TMUX_PLUGIN_MANAGER_PATH ~/.config/tmux/plugins/


########## alias ##########

alias e 'emacsclient -nw -a ""'
alias ekill 'emacsclient -e "(kill-emacs)"'
alias g 'git'
alias fzf 'fzf-tmux'


########## plugins ##########

if not functions -q fisher
    set -q XDG_CONFIG_HOME
    or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end
