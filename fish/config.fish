if status --is-interactive
    ########################################
    ### ALIAS

    alias e 'emacsclient -nw -a ""'
    alias ekill 'emacsclient -e "(kill-emacs)"'
    alias g git
    alias gg 'ghq get'

    ########################################
    ### ENVIRONMENT

    # Xdg
    set -q XDG_CONFIG_HOME
    or set -l XDG_CONFIG_HOME ~/.config

    # Path
    set -l local_paths /usr/local/{sbin,bin} /usr/{sbin,bin} /{sbin,bin}

    ## go
    set -gx GOPATH $HOME/.go
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
    set -gx TMUX_PLUGIN_MANAGER_PATH $XDG_CONFIG_HOME/tmux/plugins/
    set -gx FZF_LEGACY_KEYBINDINGS 0
    set -gx FZF_DEFAULT_OPTS '--cycle --reverse --bind "?:toggle-preview"'
    set -gx FZF_FIND_FILE_OPTS '--preview "head -100 {}"'
    set -gx FZF_CD_OPTS '--preview "tree -C {} | head -n100"'
    set -gx FZF_CD_WITH_HIDDEN_OPTS $FZF_CD_OPTS
    set -gx FZF_TMUX 1
    set -gx FZF_ENABLE_OPEN_PREVIEW 1
    set -gx GHQ_SELECTOR fzf-tmux

    ########################################
    ### PLUGINS

    if not functions -q fisher
        curl -sL https://git.io/fisher | source && fisher update
    end
end
