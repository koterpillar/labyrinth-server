Labyrinth - Server
==================

This is a server for Labyrinth. For the game rules, please see
[the Labyrinth project](https://github.com/koterpillar/labyrinth).

Live instance
-------------

[Labyrinth](http://labyrinth.koterpillar.com:8000/)

Installation
------------

Prerequisites:
* [Haskell platform](http://www.haskell.org/platform/)

Generally, the
[version on Hackage](http://hackage.haskell.org/package/labyrinth-server) is
fresh enough. To get that:

    cabal update
    cabal install labyrinth-server
    PATH=~/.cabal/bin:$PATH # This can go into ~/.profile
    labyrinth-server

To install directly from Git,
[Cabal-dev](http://hackage.haskell.org/package/cabal-dev) is recommended.

    cabal update
    cabal install cabal-dev
    cabal-dev install-deps
    cabal-dev configure
    cabal-dev build
    ./dist/build/labyrinth-server/labyrinth-server
