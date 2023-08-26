# dotfiles

[![CI](https://github.com/kuwata0037/dotfiles/actions/workflows/ci.yaml/badge.svg)](https://github.com/kuwata0037/dotfiles/actions/workflows/ci.yaml)

kuwata0037's dotfiles managed by [chezmoi](https://github.com/twpayne/chezmoi).

## Install

```sh
sh -c "$(curl -fsLS chezmoi.io/get)" -- init --apply kuwata0037
```

## Setting

```sh
echo '/home/linuxbrew/.linuxbrew/bin/fish' | sudo tee -a /etc/shells
chsh -s /home/linuxbrew/.linuxbrew/bin/fish

make completion
```

## License

- [MIT](LICENSE)
