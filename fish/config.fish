########## env ##########

if status --is-login
    ##### locale #####
    set -gx LANG en_US.UTF-8

    ##### path #####
    set -l local_paths /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

    # go
    set -gx GOPATH $HOME/dev
    if test -d $GOPATH/bin
        set local_paths $GOPATH/bin $local_paths
    end

    # ghq
    set -gx GHQ_ROOT ~/dev/src

    # rust
    if test -d ~/.cargo/bin
        set local_paths ~/.cargo/bin $local_paths
    end

    # pyenv
    if test -d ~/.pyenv/bin
        set local_paths ~/.pyenv/bin $local_paths
    end
    if which pyenv >/dev/null
        set local_paths ~/.pyenv/shims $local_paths
        source (pyenv init - | psub)
    end

    # rbenv
    if test -d ~/.rbenv/bin
        set local_paths ~/.rbenv/bin $local_paths
    end
    if which rbenv >/dev/null
        set local_paths ~/.rbenv/shims $local_paths
        source (rbenv init - | psub)
    end

    # update PATH via fish_user_paths
    set -g fish_user_paths $local_paths

    ##### misc #####
    set -gx TERM xterm-256color
    set -gx TMUX_PLUGIN_MANAGER_PATH ~/.config/tmux/plugins/
end

########## alias ##########

alias e 'emacsclient -nw -a ""'
alias ekill 'emacsclient -e "(kill-emacs)"'
alias g 'git'
alias fzf 'fzf-tmux'

########## plugins ##########

if not test -f ~/.config/fish/functions/fisher.fish
    echo "Installing fisherman for the first time"
    curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
    fisher
end
