# My xmonad config

This is my xmonad configurations using custom build script to build my xmonad executable by cabal.

## Setup

```sh
# Install dependencies.
$ pacman -S git xorg-server xorg-apps xorg-xinit xorg-xmessage libx11 libxft libxinerama libxrandr libxss pkgconf

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
