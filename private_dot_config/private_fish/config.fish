#
# Alias
#
### Emacs
alias e 'emacsclient -nw -a ""'
alias ekill 'emacsclient -e "(kill-emacs)"'

### Git
alias g git
alias gg 'ghq get'

### File
if command -qs bat
    alias cat bat
end

### Directory
if command -qs exa
    alias ls 'exa --icons'
    alias ll 'exa --icons -lh'
    alias la 'exa --icons -alh'
end
alias mk 'command mkdir -p'

#
# Environment
#
### XDG
set -q XDG_CONFIG_HOME
or set -l XDG_CONFIG_HOME ~/.config

### Path
set -l local_paths /usr/local/{sbin,bin} /usr/{sbin,bin} /{sbin,bin}

# Go
set -gx GOPATH $HOME/.go
set local_paths $GOPATH/bin $local_paths

    # Homebrew
    if test (uname -s) = Linux
        set -gx HOMEBREW_PREFIX /home/linuxbrew/.linuxbrew
        set -gx HOMEBREW_CELLAR $HOMEBREW_PREFIX/Cellar
        set -gx HOMEBREW_REPOSITORY $HOMEBREW_PREFIX/Homebrew
        set -gx MANPATH $HOMEBREW_PREFIX/share/man $MANPATH
        set -gx INFOPATH $HOMEBREW_PREFIX/share/info $INFOPATH
        set local_paths $HOMEBREW_PREFIX/{sbin,bin}
    end

# JavaScript
set -gx VOLTA_HOME $HOME/.volta
set local_paths $VOLTA_HOME/bin $local_paths

# Python
set -gx POETRY_VIRTUALENVS_IN_PROJECT true

# Rust
set local_paths ~/.cargo/bin $local_paths
if command -qs sccache
    set -gx RUSTC_WRAPPER (which sccache)
end

# update PATH via fish_user_paths
set -g fish_user_paths $local_paths

### FZF
set -gx FZF_TMUX 1
set -gx FZF_ENABLE_OPEN_PREVIEW 1
set -gx FZF_DEFAULT_OPTS '--cycle --reverse --bind "?:toggle-preview"'
set -gx FZF_FIND_FILE_OPTS '--preview "head -100 {}"'
set -gx FZF_CD_OPTS '--preview "tree -C {} | head -n100"'
set -gx FZF_CD_WITH_HIDDEN_OPTS $FZF_CD_OPTS
set -gx FZF_LEGACY_KEYBINDINGS 0

### Misc
set -gx LANG en_US.UTF-8
set -gx TERM xterm-256color
set -gx GHQ_SELECTOR fzf-tmux

#
# Plugins
#
if not functions -q fisher
    curl -sL https://git.io/fisher | source && fisher update
end

#
# Appearance
#
starship init fish | source
