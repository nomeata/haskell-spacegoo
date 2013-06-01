{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards #-}

-- | 
-- This module provides you with everything to quicky write clients for the
-- GPN13 Spacegoo programming contest. Essentially you write a function of type
-- 'Strategy' that takes the 'State' and may return a 'Move'. If you pass such
-- a function to 'clients', you are good to go. See the examples section for
-- some examples.

module Game.Spacegoo (
    -- * The state
    PlayerId(..),
    Round(..),
    Units(..),
    Coord(..),
    Player(..),
    Fleet(..),
    Planet(..),
    State(..),
    -- * Moves
    Move(..),
    Strategy(..),
    -- * Writing clients
    client,
    -- * Utilities
    -- Convenience functions for working with the state
    me,
    he,
    opponentName,
    battle,
    winsAgainst,
    distance,
    hasMore,
    ownerAt,
    linInt,
    nemesisOf,
    minimizeUnits,
    -- * Example strategies
    -- These are some simple strategies, for demonstration purposes.
    nop,
    attackNeutral,
    sendSomewhere,
    intercept,
    ) where

import Data.List (sortBy)
import Data.Ord
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Conduit
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.List as C
import Data.Conduit.Network
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Control.Monad.Trans
import Data.Foldable (forM_, find)
import qualified Data.Foldable as F 
import Data.Aeson
import Control.Applicative
import Control.Monad
import Text.Show.Pretty
import Text.PrettyPrint (render)
import qualified Data.Vector as V
import Text.Printf
--import Control.Monad.Random
import Data.Maybe
import Data.VectorSpace


data Player = Player
    { itsme :: Bool
    , playerId :: PlayerId
    , name :: Text
    }
    deriving Show

instance FromJSON Player where
    parseJSON (Object v) = 
        Player <$> v .:? "itsme" .!= False
               <*> v .: "id"
               <*> v .: "name"

-- | The player (0,1 or 2)
type PlayerId = Int

-- | A Round count
type Round = Int

-- | A position on the map
type Coord = (Int, Int)

-- | Units, either on a planet, on a fleet, or as a production indication.
type Units = (Int, Int, Int)

type FUnits = (Double, Double, Double)


fleetFromVector :: Monad m => V.Vector Int -> m Units
fleetFromVector v | V.length v /= 3 = fail "Wrong number of elements in array"
                  | otherwise       = return (v V.! 0, v V.! 1, v V.! 2)


data Fleet = Fleet
    { fleetId :: Int
    , fleetOwner :: PlayerId
    , origin :: Int
    , target :: Int
    , fleetShips :: Units
    , eta :: Int
    }
    deriving Show

instance FromJSON Fleet where
    parseJSON (Object v) = 
        Fleet <$> v .: "id"
              <*> v .: "owner_id"
              <*> v .: "origin"
              <*> v .: "target"
              <*> (fleetFromVector =<< v .: "ships")
              <*> v .: "eta"

data Planet = Planet
    { planetId :: Int
    , position :: Coord
    , planetOwner :: PlayerId
    , production :: Units
    , planetShips :: Units
    } 
    deriving Show

instance FromJSON Planet where
    parseJSON (Object v) = 
        Planet <$> v .: "id"
               <*> ( (,) <$> v .: "x" <*> v .: "y" )
               <*> v .: "owner_id"
               <*> (fleetFromVector =<< v .: "production")
               <*> (fleetFromVector =<< v .: "ships")

data State = State 
    { gameOver :: Bool
    , currentRound :: Round
    , maxRounds :: Round
    , players :: [Player]
    , fleets :: [Fleet]
    , planets :: [Planet]
    } 
    deriving Show

instance FromJSON State where
    parseJSON (Object v) = 
        State <$> v .: "game_over"
              <*> v .: "round"
              <*> v .: "max_rounds"
              <*> v .: "players"
              <*> v .: "fleets"
              <*> v .: "planets"

-- | A Move contains the id of a source planet, the id or a target planet, and
-- the number of ships to send
type Move = Maybe (Int, Int, Units)

type Strategy = State -> Move

serializeMove :: Move -> ByteString
serializeMove Nothing = "nop\n"
serializeMove (Just (from, to, (a,b,c))) =
    BS.pack $ printf "send %d %d %d %d %d\n" from to a b c

-- | This is your main entry point to play one round of the game.
client ::
    Int         -- ^ Port 
    -> String   -- ^ Hostname 
    -> String   -- ^ Username
    -> String   -- ^ Passwort
    -> Strategy -- ^ Your strategy
    -> IO ()
client port server username password player = do
    runTCPClient (clientSettings port (BS.pack server)) $ \appData -> do
        appSource appData
            $= C.decode C.utf8
            $= C.lines
            $= C.encode C.utf8
            $= disconnect
            $= parseState 
            -- not $= logState
            $= C.iterM (putStrLn . stateSummary)
            $= conduit
            $$ appSink appData
  where
    conduit = do
        yield $ BS.pack $ printf "login %s %s\n" username password
        C.map player
            =$= C.iterM (F.mapM_ putStrLn . moveSummary)
            =$= C.map serializeMove

logState :: Conduit State IO State
logState = awaitForever $ \s -> do
    liftIO $ putStrLn (render (ppDoc s))
    yield s

disconnect :: Conduit ByteString IO ByteString
disconnect = do 
    v <- await
    F.forM_ v $ \s ->
        unless (s == "game is over. please disconnect") $ do
            yield s
            disconnect

parseState :: Conduit ByteString IO State
parseState = do
    awaitForever $ \line ->
        if "{" `BS.isPrefixOf` line
        then do
            --liftIO $ putStrLn "Got State"
            case decode (BSL.fromStrict line) of
                Just s -> yield s
                Nothing -> liftIO $ BS.putStrLn $ "Failed to parse: " <>  line
        else 
            when (line `notElem` boring) $
                liftIO $ BS.putStrLn $ "Server: " <>  line
    where boring = [ "waiting for you command"
                   , "command received. waiting for other player..."
                   , "calculating round" ]
    
stateSummary :: State -> String
stateSummary State{..} =
    printf "[Round %3d/%3d]" currentRound maxRounds
    ++ " We: " ++ statsFor me ++ " He: " ++ statsFor he ++ " Neutral: " ++ statsFor 0
  where
    Just me = playerId <$> find itsme players
    he = 3 - me
    statsFor i = printf  "%2dp" (length ps) ++
                 printf " %5ds" (sum (map (unitSum . planetShips) ps) +
                                 sum (map (unitSum . fleetShips) fs)) ++
                 (if i == 0 then "" else printf " %2df" (length fs))
      where ps = filter (\p -> planetOwner p == i) planets
            fs = filter (\f -> fleetOwner f == i) fleets

moveSummary :: Move -> Maybe String
moveSummary Nothing = Nothing
moveSummary (Just (from, to, u)) = Just $
    printf "                %d -> %d (%s)" from to (show u)

unitSum :: Units -> Int
unitSum (a,b,c) = a + b + c



-- | The dead man strategy. Usually not very effective.
nop :: Strategy
nop = const Nothing

-- | From any own planet, send all ships to any opposing planet.
sendSomewhere :: Strategy
sendSomewhere (State {..}) = do
    me <- playerId <$> find itsme players
    let he = 3 - me
    aPlanet <- find (\p -> planetOwner p == me && planetShips p /= (0,0,0)) planets
    otherPlanet <- find (\p -> planetOwner p == he) planets
    return (planetId aPlanet, planetId otherPlanet, planetShips aPlanet)

-- | Picks an own planet with a reasonable number of ships and sends it to some
-- neutral planet.
attackNeutral :: Strategy
attackNeutral (State {..}) = do
    me <- playerId <$> find itsme players
    aPlanet <- find (\p -> planetOwner p == me && strong p) planets
    otherPlanet <- find (\p -> planetOwner p == 0) planets
    return (planetId aPlanet, planetId otherPlanet, planetShips aPlanet)
  where strong p = vSum (planetShips p) > 10 * vSum (production p)
        vSum = ((1,1,1) <.>)
    
-- | Look for an opposing fleet. If we have a planet with more ships than the
-- opposing fleet that would arrive shortly after that, send a fleet the same size
-- as the opposing fleet.
intercept :: Strategy
intercept (State {..}) = do
    me <- playerId <$> find itsme players
    let he = 3 - me
    msum $ flip map fleets $ \f -> do
        guard $ fleetOwner f == he
        let Just t = find (\p -> planetId p == target f) planets
        msum $ flip map planets $ \p -> do
            guard $ planetOwner p == me
            guard $ planetShips p `hasMore` fleetShips f
            guard $ currentRound + distance p t - eta f `elem` [1,2]
            return $ (planetId p, planetId t, fleetShips f)

-- | Whether the first player has at least as many ships as the other
hasMore :: Units -> Units -> Bool
hasMore (a,b,c) (a',b',c') = a >= a && b >= b' && c >= c'

float2 :: (Int, Int) -> (Double, Double)
float2 (a,b) = (fromIntegral a, fromIntegral b)

map3 :: (a->b) -> (a,a,a) -> (b,b,b)
map3 f (a,b,c) = (f a, f b, f c)

float3 :: Units -> FUnits
float3 = map3 fromIntegral

floor3 :: FUnits -> Units
floor3 = map3 floor

nonneg3:: FUnits -> FUnits
nonneg3 = map3 (max 0)

distance :: Planet -> Planet -> Int
distance p1 p2 = ceiling (magnitude (float2 (position p1 ^-^ position p2)))

linInt :: Double -> Units -> Units -> Units
linInt f u1 u2 = floor3 (lerp (float3 u1) (float3 u2) f)

-- | My id
me :: State -> Int
me s = fromJust $ playerId <$> find itsme (players s)

-- | The other players id
he :: State -> Int
he s = 3 - me s 

-- | The opponent's name; to filter out known bad opponents
opponentName :: State -> Text
opponentName s = fromJust $ 
    name <$> find (not . itsme) (players s)

damage :: FUnits -> FUnits
damage (a,b,c)  = ( 0.25 * c + (if c > 0 then 2 else 0)
                  + 0.1  * a + (if a > 0 then 1 else 0) 
                  + 0.01 * b + (if b > 0 then 1 else 0) 
                  , 0.25 * a + (if a > 0 then 2 else 0)
                  + 0.1  * b + (if b > 0 then 1 else 0) 
                  + 0.01 * c + (if c > 0 then 1 else 0) 
                  , 0.25 * b + (if b > 0 then 2 else 0)
                  + 0.1  * c + (if c > 0 then 1 else 0) 
                  + 0.01 * a + (if a > 0 then 1 else 0) 
                  ) 

oneRound :: FUnits -> FUnits -> FUnits
oneRound att def = nonneg3 $ def ^-^ damage att

-- | Whether the first argument wins against the second, and how many ships are
-- left
battle :: Units -> Units -> (Bool, Units)
battle att def = go (float3 att) (float3 def)
    where go a d | magnitude a <= 1e-5 = (False, floor3 d)
                 | magnitude d <= 1e-5 = (True, floor3 a)
                 | otherwise = go (oneRound d a) (oneRound a d)

-- | Whether the first fleet wins against the second (defaulting to the second)
winsAgainst :: Units -> Units -> Bool
winsAgainst att def = fst (battle att def)


-- | Predict the owner and strength of the planet at the given round
ownerAt :: State -> Int -> Round ->  (PlayerId, Units) 
ownerAt s i round = go (currentRound s, planetOwner p, planetShips p) $
    sortBy (comparing eta) $
    filter (\f -> target f == i) $
    filter (\f -> eta f <= round) $
    fleets s
  where
    go (r, o, ships) [] = (o, produce o ships (round - r))
    go (r, o, ships) (f:fs)
        | fleetOwner f == o
        = go (eta f, o, produce o ships (eta f - r) ^+^ fleetShips f) fs
        | fleetOwner f /= o
        = case battle (fleetShips f) (produce o ships (eta f - r)) of
            (True, rest) ->  go (eta f, fleetOwner f, rest) fs
            (False, rest) -> go (eta f, o, rest) fs
    Just p = find (\p -> planetId p == i) (planets s)
    produce 0 ships _ = ships  
    produce _ ships n = ships ^+^ n *^ production p

nemesisOf :: Units -> Units
nemesisOf (a,b,c) = (b,c,a)


-- If the attacker wins against the defender, try to find a subset that also wins.
minimizeUnits :: Units -> Units -> Units
minimizeUnits a d = go a a 
  where
    go last a = case battle a d of
                    (True, r)  -> let r' = map3 (`div` 3) (a ^-^ r)
                                  in if r' /= (0,0,0) then go a (a ^-^ r') else a
                    (False, _) -> last

