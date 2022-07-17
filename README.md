# My xmonad config

[![CI](https://github.com/a5ob7r/xmonad-config/actions/workflows/ci.yaml/badge.svg)](https://github.com/a5ob7r/xmonad-config/actions/workflows/ci.yaml)
[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

This is my xmonad configurations using custom build script to build my xmonad executable by cabal.

## Setup

```sh
# Install dependencies to build xmoand.
$ pacman -S xorg-server xorg-apps xorg-xinit xorg-xmessage libx11 libxft libxinerama libxrandr libxss pkgconf

# Install dependencies for others.
$ pacman -S git feh picom light alsa-utils autorandr imagemagick

# Clone this repository.
$ git clone https://github.com/a5ob7r/xmonad-config.git
$ cd xmonad-config

# Deploy configs.
$ ln -sfv "$PWD" ~/.config/xmonad
$ ln -sfv "$PWD/xmobar" ~/.config/xmobar

# Only install the xmonad executable, not as a library, to manage xmonad.
$ cabal install xmonad

# Install xmobar.
$ cabal install xmobar --overwrite-policy=always --flags="all_extensions"

# Build and install my xmonad executable.
$ xmonad --recompile

# Append a xmonad command to .xinitrc.
$ echo 'exec xmonad' >> ~/.xinitrc
```
