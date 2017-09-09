# path
set -g fish_user_paths "/usr/local/bin" $fish_user_paths

# alias
alias e='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)"'

# plugins
if not test -f ~/.config/fish/functions/fisher.fish
    echo "Installing fisherman for the first time"
    curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
    fisher
end
