_Headroom_ is written in [Haskell][web:haskell], you can either use pre-built binaries, or build in on your own from source code.

## Using Homebrew
If you use _macOS_, you can install Headroom easily from [Homebrew][web:homebrew] using the following command:

```
$ brew install norcane/tools/headroom
```

## Using Pre-built Binaries
Pre-built binaries _(x64)_ are available for _GNU/Linux_ and _macOS_ and can be downloaded for selected release from [releases page][meta:releases].

## From Source Code
Headroom is written in [Haskell][web:haskell], so you can install it from source code either using [Cabal][web:cabal] or [Stack][web:stack].

!!! note
    You might need to install the _pcre C library_ dependencies, if you're getting errors during the build. This can be done either by `brew install pcre` on _macOS_, or `apt-get install libpcre3-dev` (or similar) on _GNU/Linux_.

### Using Cabal
1. install [Cabal][web:cabal] for your platform
1. run `cabal install headroom`
1. add `$HOME/.cabal/bin` to your `$PATH`

### Using Stack
1. install [Stack][web:stack] for your platform
1. clone this repository
1. run `stack install` in the project directory
1. add `$HOME/.local/bin` to your `$PATH`


[meta:releases]: https://github.com/xwinus/headroom/releases
[web:cabal]: https://www.haskell.org/cabal/
[web:haskell]: https://haskell.org
[web:homebrew]: https://brew.sh
[web:stack]: https://www.haskellstack.org
