# Wrapping a C library

## Cabal

```bash
$ nix-shell
$ cabal build
```

> nix-shell is only required on NixOS.

## Nix

```bash
$ nix-build -A haskell-ffi
```

## Troubleshooting

If cabal fails due to a `missing pcre.h`, you need to check if the library is installed in your distribution
and install it

``` bash
pkg-config --list-all | grep 'libpcre'

# Arch linux
pacman -Syu pcre
```

## Credits

- http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html
- https://github.com/haskell-hvr/regex-pcre

