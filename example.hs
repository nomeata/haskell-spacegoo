import Game.Spacegoo
import Data.List (find)

main = client 8000 "http://spacegoo.rent-a-geek.de/" "username" "password" myStrategy

myStrategy s = do
    me <- playerId `fmap` find itsme (players s)
    let he = 3 - me
    aPlanet <- find (\p -> planetOwner p == me) (planets s)
    otherPlanet <- find (\p -> planetOwner p == he) (planets s)
    return (planetId aPlanet, planetId otherPlanet, planetShips aPlanet)

