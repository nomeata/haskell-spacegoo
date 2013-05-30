{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards #-}

-- | 
-- This module provides you with everything to quicky write clients for the
-- GPN13 Spacegoo programming contest. Essentially you write a function of type
-- 'Strategy' that takes the 'State' and may return a 'Move'. If you pass such
-- a function to 'clients', you are good to go. See the examples section for
-- some examples.

module Game.Spacegoo (
    -- * The state
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
    -- * Example strategies
    -- These are some simple strategies, for demonstration purposes.
    nop,
    attackNeutral,
    sendSomewhere,
    intercept,
    ) where

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
    , playerId :: Int
    , name :: Text
    }
    deriving Show

instance FromJSON Player where
    parseJSON (Object v) = 
        Player <$> v .:? "itsme" .!= False
               <*> v .: "id"
               <*> v .: "name"

-- | A position on the map
type Coord = (Int, Int)

-- | Units, either on a planet, on a fleet, or as a production indication.
type Units = (Int, Int, Int)


fleetFromVector :: Monad m => V.Vector Int -> m Units
fleetFromVector v | V.length v /= 3 = fail "Wrong number of elements in array"
                  | otherwise       = return (v V.! 0, v V.! 1, v V.! 2)


data Fleet = Fleet
    { fleetId :: Int
    , fleetOwner :: Int
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
    , planetOwner :: Int
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
    , round :: Int
    , maxRounds :: Int
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

nopClient =  client 6000 "spacegoo.rent-a-geek.de" "foo" "bar" (const Nothing)

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
    printf "[Round %3d/%3d]" round maxRounds
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
    aPlanet <- find (\p -> planetOwner p == me) planets
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
            guard $ round + distance p t - eta f `elem` [0,1,2]
            return $ (planetId p, planetId t, fleetShips f)

hasMore :: Units -> Units -> Bool
hasMore (a,b,c) (a',b',c') = a >= a && b >= b' && c >= c'
        
float2 :: (Int, Int) -> (Double, Double)
float2 (a,b) = (fromIntegral a, fromIntegral b)

float3 :: (Int, Int, Int) -> (Double, Double, Double)
float3 (a,b,c) = (fromIntegral a, fromIntegral b, fromIntegral c)

distance :: Planet -> Planet -> Int
distance p1 p2 = ceiling (magnitude (float2 (position p1 ^-^ position p2)))
