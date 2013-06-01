import Test.QuickCheck
import Game.Spacegoo

map3 :: (a->b) -> (a,a,a) -> (b,b,b)
map3 f (a,b,c) = (f a, f b, f c)

nemesisWins :: Units -> Property
nemesisWins u = (map3 (max 0) u == u) ==> winsAgainst u (nemesisOf u) == False

minimizeUnitsWins :: Units -> Units -> Property
minimizeUnitsWins a d = winsAgainst a' d' ==> winsAgainst (minimizeUnits a' d') d'
  where
    (a',d') | winsAgainst (map3 abs a) (map3 abs d) = (map3 abs a, map3 abs d)
            | otherwise                             = (map3 abs d, map3 abs a)

