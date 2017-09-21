########## env ##########

if status --is-login
    ### locale ###
    set -gx LANG en_US.UTF-8

    ### path ###
    set -g fish_user_paths /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

    # for go
    if test (which go)
        set -gx GOPATH $HOME
        set -g fish_user_paths $GOPATH/bin $fish_user_paths
    end

    set -gx GHQ_ROOT $GOPATH/src

    # for pyenv
    if test -d $HOME/.pyenv
        test -d $HOME/.pyenv/bin
        and set fish_user_paths $HOME/.pyenv/bin $fish_user_paths

        source (pyenv init - | psub)
    end

    # for rbenv
    if test -d $HOME/.rbenv
        test -d $HOME/.rbenv/bin
        and set fish_user_paths $HOME/.rbenv/bin $fish_user_paths

        source (rbenv init - | psub)
    end
end

########## alias ##########

alias e='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)"'
alias g='git'
alias fzf='fzf-tmux'

########## plugins ##########

if not test -f ~/.config/fish/functions/fisher.fish
    echo "Installing fisherman for the first time"
    curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
    fisher
end
