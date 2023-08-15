#!/usr/bin/env bash

install_system_package() {
    if type apt-get >/dev/null 2>&1; then
        sudo apt-get update
        sudo apt-get install -y \
            language-pack-ja \
            libgit2-dev \
            libssh-dev \
            libssl-dev \
            pkg-config \
            wslu \
            xdg-utils
    fi
}

install_homebrew() {
    if ! (type brew >/dev/null 2>&1); then
        # Install requirements
        # see: https://docs.brew.sh/Homebrew-on-Linux#requirements
        if [ "$(uname)" == 'Darwin' ]; then
            xcode-select --install
        elif type apt-get >/dev/null 2>&1; then
            sudo apt-get update
            sudo apt-get install -y build-essential procps curl file git
        elif type yum >/dev/null 2>&1; then
            sudo yum groupinstall 'Development Tools'
            sudo yum install -y procps-ng curl file git
        else
            echo "Unsupported OS. Please install manually."
            return 1
        fi

        # Install Homebrew
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

        # Install bundle
        type brew >/dev/null 2>&1 && brew bundle --global
        test -d /opt/homebrew && /opt/homebrew/bin/brew bundle --global
        test -d ~/.linuxbrew && ~/.linuxbrew/bin/brew bundle --global
        test -d /home/linuxbrew/.linuxbrew && /home/linuxbrew/.linuxbrew/bin/brew bundle --global
    fi
}

install_rust() {
    if ! (type rustc >/dev/null 2>&1); then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path
    else
        echo "You have already installed Rust."
    fi
}

install_cargo_subcommand() {
    if type ~/.cargo/bin/cargo >/dev/null 2>&1; then
        curl -L --proto '=https' --tlsv1.2 -sSf \
            https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash
        ~/.cargo/bin/cargo binstall -y \
            cargo-audit \
            cargo-edit \
            cargo-llvm-cov \
            cargo-make \
            cargo-nextest \
            cargo-sort \
            cargo-udeps \
            cargo-update \
            cargo-watch
    else
        echo "Cargo is not installed. Please install manually."
    fi
}

install_google_cloud_sdk() {
    if ! (type gcloud >/dev/null 2>&1); then
        mkdir -p ~/.google-cloud-sdk
        curl -fSL https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-381.0.0-linux-x86_64.tar.gz | tar zx -C ~/.google-cloud-sdk/ --strip-components 1
    else
        echo "You have already installed google cloud sdk."
    fi
}

main() {
    install_system_package
    install_homebrew
    install_rust
    install_cargo_subcommand
    install_google_cloud_sdk
}

main
