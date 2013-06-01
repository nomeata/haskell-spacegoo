{-# LANGUAGE OverloadedStrings #-}

-- This is the complete and final code of nomeata's bot (nomeataintercept2 in
-- the game). Feel free to use it for your own bot. If you are successful with
-- it, let me know. Or give it a name that ends with "2n". Or both.
--
-- 2013 Joachim Breitner <mail@joachim-breitner.de>

import Game.Spacegoo

import Control.Monad
import Data.List
import Data.VectorSpace
import Data.Ord

-- Client is provided by Game.Spacegoo and takes a port, a hostname, a
-- username, a password and a function that takes the state of the game and
-- returns a Maybe Move
main :: IO ()
main = client 6000 "spacegoo.gpn.entropia.de" "username" "password" $ \s ->
    -- msum takes a list of (Maybe Move) and returns the first Move found (or
    -- Nothing if none of the strategy returns a move
    --
    -- Here we combine four strategies
    msum [ guard (currentRound s > 50) >> cheapNeutral s
           -- The guard prevents the strategy afterwards from being considered
           -- if the condition does not hol
         , fullForceOn (he s) s
         , tickleNearNeutral s
         , guard (currentRound s > 200) >> fullForceOn 0 s
         , guard (currentRound s > 100) >> tickle (he s) s
         ]

-- Strategy 1: Try to capture neutral planets that
--  * we can definitely conquer
--  * are much closer to us than to the enemy
cheapNeutral :: Strategy
cheapNeutral s = do
    pickLowest [ (d , (planetId p, planetId op, minimizeUnits (planetShips p) goal))
        | p <- planets s
        -- Iterate through all our planets
        , planetOwner p == me s
        -- and look at each other planet
        , op <- planets s
        , planetId p /= planetId op
        -- Check who owns the planet when we would reach it (o'), and how much
        -- ships are there (goal)
        , let (o', goal) = ownerAt s (planetId op)
                                     (currentRound s + distance p op + 2)
        -- Make sure that the planet is still neutral then...
        , o' == 0
        -- ..that we can win it..
        , planetShips p `winsAgainst` goal
        , let d = distance p op
        -- And that the planet is far from the enemy
        , and [ distance op p2' > 2 * d
              | p2' <- planets s, planetOwner p2' == he s
              ]
        ]

-- Strategy 2+4: If we can conquer a planet owned by the specified player, do it.
fullForceOn :: Int ->  Strategy
fullForceOn victim s = do
    -- We send the smallest number of ships that still kills it; possibly
    -- chosing a good type (see Game.Spacegoo.minimizeUnits)
    pickLowest [ (d, (planetId p, planetId op, minimizeUnits (planetShips p) goal)) 
        | p <- planets s
        -- p is our planet
        , planetOwner p == me s
        , op <- planets s
        , planetId p /= planetId op
        -- op is the target
        , let d = distance p op
        , let (o', goal) = ownerAt s (planetId op)
                                     (currentRound s + d + 2)
        , o' == victim
        -- and we really win against it
        , planetShips p `winsAgainst` goal
        ]

-- Strategy 3: Reduce the strength of nearby neutral planets with (1,1,1)
-- fleets (which are very effective)
tickleNearNeutral :: Strategy
tickleNearNeutral s = do
    pickLowest [ (d, (planetId p, planetId op, (1,1,1)))
        | p <- planets s
        , planetOwner p == me s
        , op <- planets s
        , planetId p /= planetId op
        , let (o', _) = ownerAt s (planetId op)
                                     (currentRound s + distance p op + 2)
        , o' == 0
        , let d = distance p op
        , and [ distance op p2' > 2*d
              | p2' <- planets s, planetOwner p2' == he s
              ]
        ]


-- | Strategy 5: Send (1,1,1) fleets to the planet of the enemy with the
-- smallest number of ships
tickle :: Int ->  Strategy
tickle victim s = do
    -- Pick the other planet
    otherPlanet <- pickLowest [ ( magnitudeSq (planetShips p) , p)
                              | p <- planets s, planetOwner p == victim ]

    -- Pick our planet (the closest one with ships)
    myPlanet <- pickLowest [ (distance p otherPlanet, p)
                           | p <- planets s, planetOwner p == me s, planetShips p /= (0,0,0) ]

    return (planetId myPlanet, planetId otherPlanet, (1,1,1))


-- Utility function: Pick the tuple with the lowest entry in the first
-- component and return its second component
pickLowest :: Ord a => [(a,b)] -> Maybe b
pickLowest [] = Nothing
pickLowest l = Just (snd (minimumBy (comparing fst) l))
