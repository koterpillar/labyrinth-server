Labyrinth - Server
==================

This is a server for Labyrinth. For the game rules, please see
[the Labyrinth project](https://github.com/koterpillar/labyrinth).

Installation
------------

Prerequisites:
* [Haskell platform](http://www.haskell.org/platform/)

Generally, the
[version on Hackage](http://hackage.haskell.org/package/labyrinth-server) is
fresh enough. To get that:

    cabal install labyrinth-server

To install directly from Git,
[Cabal-dev](http://hackage.haskell.org/package/cabal-dev) is recommended.

    cabal install cabal-dev
    cabal-dev install-deps
    cabal-dev configure
    cabal-dev build
