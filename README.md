# XMonad-Config
My personal config for XMonad window manager and related software.

## External Dependencies

TBD.

## Install command

Clone or link repository to ~/.xmonad:

```
cd ~/.xmonad
cabal v2-install xmonad-setup --overwrite-policy=always
```

Launch:
```
startx ~/.xmonad/xmonad-start
```

### If everything goes wrong

Clear current user packages and update:
```
rm -rf ~/.cabal
rm -rf ~/.ghc
cabal v2-update
echo ':q' | cabal v2-repl
```

Install minimal dependencies
```
cabal v2-install xmonad xmonad-contrib --lib;
```

Build minimal interface
```
ghc --make\
    -no-keep-hi-file\
    -no-keep-o-file\
    -i.xmonad/src\
    -o .xmonad/xmonad-x86_64-linux\
    .xmonad/src/xmonad.hs
```
