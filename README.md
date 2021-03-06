Haskell Spacegoo
================

Using this library you can quickly create code to take part in a game of Rocket
Scissor Spacegoo; see <https://bitbucket.org/dividuum/rocket-scissor-spacegoo>
for more details on the game.

To install this library, just call `cabal install haskell-spacegoo` (and
`apt-get install cabal-install` first if required). Then you can write your
Haskell clients.

Here is a minimal example, that just sends all ships from one planet to an
enemy planet:

    import Game.Spacegoo
    import Data.List (find)
    
    main = client 8000 "spacegoo.rent-a-geek.de" "username" "password" myStrategy
    
    myStrategy s = do
        aPlanet <- find (\p -> planetOwner p == me s) (planets s)
        otherPlanet <- find (\p -> planetOwner p == he s) (planets s)
        return (planetId aPlanet, planetId otherPlanet, planetShips aPlanet)

See the documentation of the
[`Game.Spacegoo`](http://hackage.haskell.org/packages/archive/haskell-spacegoo/latest/doc/html/Game-Spacegoo.html)
modules for more information on how to write strategies, and for more examples.
