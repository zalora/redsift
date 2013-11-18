## Redsift: 

Web interface to redshift for exploring the data

## Pre-requisites

* postgresql:
    
On OS X:
    
    brew install --no-tcl postgresql    

On Ubuntu:
    
    sudo apt-get install postgresql-client

With ~/.pgpass:

    <host>:<port>:<db>:<user>:<password>

* lighttpd:

On OS X: 

    brew install lighttpd

On Ubuntu:
    
    sudo apt-get install lighttpd

## Installation

Using GHC of version at least 7.4 is recommended. Also, using cabal-dev to build up a virtual environment is a good practice. To install it using cabal-install

    cabal update
    cabal install cabal-dev

Then do the following:

    git clone <this repository>
    cabal-dev install --only-dependencies

## Run

    $ lighttpd -D -f /home/location/to/lighttpd.conf
    $ redsift