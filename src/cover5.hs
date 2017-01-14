module Cover5 where

import Data.Random.Extras
import Data.Random.Show.Unsafe
import Data.Random.RVar
import Data.Maybe
import System.Environment
import Data.List
import Control.Monad.State


run :: IO ()
run = 
  do
    args <- getArgs
    let numGames = (read $ head args) 
    putStrLn $ show $ sortBy comparePicks <$> 
        evalStateT makePicks [1..numGames]

-- | An ordering function for picks. It orders on the first field
comparePicks :: (Int,Char) -> (Int,Char) -> Ordering
comparePicks (a,_) (b,_) = compare a b

-- | Makes 5 picks for cover5. Takes an int
--   which is the number of games to choose from.
--   Returns a list of pairs. Each pair indicates
--   a game id and a or b.
makePicks :: StateT [Int] RVar [(Int,Char)]
makePicks = 
  do
    g1 <- makePick
    g2 <- makePick
    g3 <- makePick
    g4 <- makePick
    g5 <- makePick
    return [g1,g2,g3,g4,g5]

-- A state transformer that selects a game and a team from a list of games.
-- The state is modified to remove the selected game from the state.
makePick :: StateT [Int] RVar (Int,Char)
makePick =
  do
    gameId <- selectGame
    teamId <- lift flipCoin
    return (gameId,teamId)


-- 'Flips' a coin. Returns 'a' or 'b'.
flipCoin :: RVar Char
flipCoin =
  do
    (_,coin) <- fromJust $ choiceExtract ['a', 'b']
    return coin


-- A state transformer that selects a game identifier 
-- from the list of identifiers
-- stored in the state. The state is modified to remove
-- the chosen identifier.
selectGame :: StateT [Int] RVar Int
selectGame = do
                gs <- get
                (gs',g) <- lift $ fromJust $ choiceExtract gs
                put gs'
                return g
