# Haskell Dice Game

## How to run

```bash
cabal update
cabal install --lib random
cabal install mtl
```

Depois realizar os passos acima, tive depois que digitar:

```bash
ghci

:m + System.Random
:set -package mtl
:l main.hs
main

:q
```



## TODO

- [ ] Colocar nomes e matrículas em todos os arquivos
