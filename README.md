# My xmonad config

[![CI](https://github.com/a5ob7r/xmonad-config/actions/workflows/ci.yaml/badge.svg)](https://github.com/a5ob7r/xmonad-config/actions/workflows/ci.yaml)
[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

This is my xmonad configurations using a custom build script to build my xmonad executable by cabal.

## Why?

We can decouple xmonad as a library from our global GHC environment by managing our xmonad configuration as just a haskell application using `cabal` and `ghcup`.

In general, we install `xmonad` and `xmonad-contrib` in our global GHC environment using `cabal install --lib`.
This means that xmonad depends on our global GHC environment, which consists of a specific version's GHC compiler and the package environment of it.
So we must change the global GHC compiler's version and install libraties to the new environment if we want to change the compiler to build xmonad, and vice versa.
And we also have to manage the GHC environments file to use the libraries in that case.
That file contains configurations about which package GHC can look for.
When install `xmonad` and `xmonad-contrib` using `cabal install --lib`, cabal probably writes appropriate package configurations to the GHC environment file.
However cabal doesn't delete old package configurations and just writes new package configurations when reinstall any package.
So we have to delete the old package configurations in our GHC environment file.
If we want to use another version of package which is installed our global environment, we must delete the old version and it may berak the global environment.
Maybe this problem can be resolved by using a local GHC environment, but we still need to modify the GHC environemnt file.
These are bothersome for us too much.

These problems can be resolved by managing our xmonad configuration as just a haskell application using `cabal` and `ghcup`.
`cabal` can switch a GHC version to build an application using `-w, --with-compier=PATH` option or `with-compiler` configuration in `cabal.project` or `cabal.project.local` without changing our global GHC environment.
In addition, we can easily install multiple GHC versions using [ghcup](https://www.haskell.org/ghcup/).
In this case, no need to modify our global or local GHC environment file to install or reinstall xmonad and other libraries, `cabal` does it automatically.

Sadly building xmonad by `cabal` doesn't support by xmoand, so we have to write a custom build script to do it.
In almost all cases, the script is a shell script.
Maybe writing and maintaining it is also bothersome if we don't like it.

[Stack](https://docs.haskellstack.org/en/stable/README/) can resolve these problems too and no need to write a custom build script because building xmonad by `Stack` is supported by xmonad.

So maybe users who match following statement (and me) want to build xmonad by `cabal`.

- Not a `Stack` user
- Don't mind to write a custom build script

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
$ cabal install --overwrite-policy=always xmonad

# Install xmobar.
$ cabal install --overwrite-policy=always --flags=all_extensions xmobar

# Build and install my xmonad executable.
$ xmonad --recompile

# Append a xmonad command to .xinitrc.
$ echo 'exec xmonad' >> ~/.xinitrc
```
