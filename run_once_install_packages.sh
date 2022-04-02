#!/usr/bin/env bash

install_homebrew() {
    if !(type brew >/dev/null 2>&1); then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    brew bundle --global
}

install_rust() {
    if !(type rustc >/dev/null 2>&1); then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    fi
}

main() {
    install_homebrew
    install_rust
}

main
