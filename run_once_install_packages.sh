#!/usr/bin/env bash

install_homebrew() {
    if !(type brew >/dev/null 2>&1); then
        if [ "$(uname)" == 'Darwin' ]; then
            /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
            brew bundle --global
        elif type apt-get >/dev/null 2>&1; then
            sudo apt-get update
            sudo apt-get install -y build-essential procps curl file git
            /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
            test -d ~/.linuxbrew && ~/.linuxbrew/bin/brew bundle --global
            test -d /home/linuxbrew/.linuxbrew && /home/linuxbrew/.linuxbrew/bin/brew bundle --global
        elif type yum >/dev/null 2>&1; then
            sudo yum groupinstall 'Development Tools'
            sudo yum install -y procps-ng curl file git
            /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
            test -d ~/.linuxbrew && ~/.linuxbrew/bin/brew bundle --global
            test -d /home/linuxbrew/.linuxbrew && /home/linuxbrew/.linuxbrew/bin/brew bundle --global
        else
            echo "Unsupported OS. Please install manually."
        fi
    fi
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