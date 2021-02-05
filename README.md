# My xmonad config

## Setup

### Package Install

```sh
# Install packages.
$ cabal install xmonad xmonad-contrib
$ cabal install xmobar --flags="all_extensions"
$ cabal install X11 --lib

# Or

# Install git version.
$ git clone https://github.com/xmonad/xmonad.git
$ git clone https://github.com/xmonad/xmonad-contrib.git
$ cabal install xmonad xmonad-contrib --overwrite-policy=always -O2 --lib

$ git clone https://github.com/jaor/xmobar.git
$ cabal install xmobar --overwrite-policy=always -O2 --lib

$ cabal install X11 --overwrite-policy=always -O2 --lib
```

### Deploy config

```sh
$ git clone https://github.com/a5ob7r/xmonad-config.git
$ ln -sfv "$PWD/xmonad-config" ~/.xmonad

# Compile xmonad binary.
$ xmonad --recompile

# Append xmonad command to .xinitrc.
$ echo exec xmonad >> ~/.xinitrc
```
