## What is Redsift

Redsift is a web interface to [Redshift](http://aws.amazon.com/redshift/)
for exploring the data.

## Requirements

Redsift is written in Haskell with [GHC](http://www.haskell.org/ghc/).
Using GHC of version at least 7.4 is recommended.

All requried Haskell libraries are listed in redsift.cabal.
Use [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install)
to fetch and build all pre-requisites automatically.

The postgresql-libpq package requires PostgreSQL client library (libpq).

## Installation

Then do the following:

    git clone https://github.com/zalora/redsift.git
    cd redsift
    cabal install

## Configuration

See sample configuration file under the Config directory.

## Run

    redsift -c <config file>
