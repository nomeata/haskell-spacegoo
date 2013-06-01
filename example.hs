import Game.Spacegoo
import Data.List (find)

main = client 8000 "spacegoo.rent-a-geek.de" "username" "password" myStrategy

myStrategy s = do
    aPlanet <- find (\p -> planetOwner p == me s) (planets s)
    otherPlanet <- find (\p -> planetOwner p == he s) (planets s)
    return (planetId aPlanet, planetId otherPlanet, planetShips aPlanet)
