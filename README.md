# Taworvor

A semi esolang. 

# Build

You need GHC (ideally with the Haskell Plattform) and you need the parsec and containers library.
If they were not shipped as part of your GHC you can install them through cabal (or possibly even
through your linux package manager).

    # cabal install parsec
    # cabal install containers
    

Building using GHC:

    ghc -o Taworvor -O3 Taworvor.hs
