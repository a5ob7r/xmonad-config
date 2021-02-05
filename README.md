# My xmonad config

## Setup

```sh
# Install packages.
$ cabal install xmonad xmonad-contrib
$ cabal install xmobar --flags="all_extensions"
$ cabal install X11 --lib

$ cd /path/to/this/repository
$ ln -sfv "${PWD}" ~/.xmonad

# Compile xmonad binary.
$ xmonad --recompile

# Append xmonad command to .xinitrc.
$ echo exec xmonad >> ~/.xinitrc
```
