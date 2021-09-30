# My xmonad config

## Setup

### Install Packages

xmonad

```sh
# Install dependencies.
$ git clone https://github.com/xmonad/xmonad-contrib.git
$ cd xmonad-contrib
$ cabal install --overwrite-policy=always --lib xmonad xmonad-contrib X11

# Install xmonad as executable for initial compilation.
$ git clone https://github.com/xmonad/xmonad.git
$ cd xmonad
$ cabal install --overwrite-policy=always
```

xmobar

```sh
$ cabal install xmobar --overwrite-policy=always --flags="all_extensions"
```

### Deploy config

```sh
$ git clone https://github.com/a5ob7r/xmonad-config.git
$ cd xmonad-config
$ ln -sfv "$PWD" ~/.config/xmonad
$ ln -sfv "$PWD/xmobar" ~/.config/xmobar

# Compile xmonad binary.
$ xmonad --recompile

# Append xmonad command to .xinitrc.
$ echo exec xmonad >> ~/.xinitrc
```
