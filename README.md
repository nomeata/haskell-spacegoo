Haskell Spacegoo
================

Using this library you can quickly create code to take part in a game of Rocket
Scissor Spacegoo; see <http://spacegoo.rent-a-geek.de/> for more details on the
game.

Here is a minimal example, that just sends all ships from one planet to an
enemy planet:

    import Game.Spacegoo
    import Data.List (find)
    
    main = client 8000 "http://spacegoo.rent-a-geek.de/" "username" "password" myStrategy
    
    myStrategy s = do
        me <- playerId `fmap` find itsme (players s)
        let he = 3 - me
        aPlanet <- find (\p -> planetOwner p == me) (planets s)
        otherPlanet <- find (\p -> planetOwner p == he) (planets s)
        return (planetId aPlanet, planetId otherPlanet, planetShips aPlanet)

See the documentation of the `Game.Spacegoo` modules for more information on
how to write strategies, and for more examples.
