########## env ##########

if status --is-login
    ##### locale #####
    set -gx LANG en_US.UTF-8

    ##### term #####
    set -gx TERM xterm-256color

    ##### path #####
    set -l local_paths /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

    # go
    set -gx GOPATH $HOME/dev
    if test -d $GOPATH/bin
        set local_paths $GOPATH/bin $local_paths
    end

    # rust
    if test -d ~/.cargo/bin
        set local_paths ~/.cargo/bin $local_paths
    end

    # pyenv
    if test -d ~/.pyenv/bin
        set local_paths ~/.pyenv/bin $local_paths
    end
    if test -d ~/.pyenv/shims
        set local_paths ~/.pyenv/shims $local_paths
    end

    # rbenv
    if test -d ~/.rbenv/bin
        set local_paths ~/.rbenv/bin $local_paths
    end
    if test -d ~/.rbenv/shims
        set local_paths ~/.rbenv/shims $local_paths
    end

    # update PATH via fish_user_paths
    set -g fish_user_paths $local_paths

    ##### misc #####
    set -gx GHQ_ROOT ~/dev/src
    set -gx TMUX_PLUGIN_MANAGER_PATH ~/.config/tmux/plugins/
end

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
