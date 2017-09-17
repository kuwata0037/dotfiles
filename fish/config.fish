########## path ##########

if status --is-login
    set -g fish_user_paths /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

    # pyenv
    if test -d $HOME/.pyenv
        set fish_user_paths $HOME/.pyenv/bin $fish_user_paths
        source (pyenv init - | psub)
    end

    # rbenv
    if test -d $HOME/.rbenv
        set fish_user_paths $HOME/.rbenv/bin $fish_user_paths
        source (rbenv init - | psub)
    end
end

########## alias ##########

alias e='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)"'
alias g='git'

########## plugins ##########

if not test -f ~/.config/fish/functions/fisher.fish
    echo "Installing fisherman for the first time"
    curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
    fisher
end
