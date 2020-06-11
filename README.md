# My xmonad config

## Setup

```sh
# Install packages.
$ cabal install xmonad xmonad-contrib
$ cabal install xmobar --flags="all_extensions"

$ cd /path/to/this/repository
$ ln -sfv "${PWD}" ~/.xmonad

# Compile xmonad binary.
$ xmonad --recompile

# Append xmonad command to .xinitrc.
$ echo exec xmonad >> ~/.xinitrc
```
