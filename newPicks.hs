import Data.Random.Extras
import Data.Random.Show.Unsafe
import Data.Random.RVar
import Data.Maybe
import System.Random
import System.Environment


main = 
  do
    args <- getArgs
    let numGames = (read $ head args) 
    putStrLn $ show $ makePicks numGames

-- 'Flips' a coin. Returns 'a' or 'b'.
flipCoin :: RVar Char
flipCoin =
  do
    (_,coin) <- fromJust $ choiceExtract ['a', 'b']
    return coin


-- Selects a game from a list of game identifiers.
selectGame :: [Int] -> RVar ([Int], Int)
selectGame = fromJust . choiceExtract 

-- Selects a game and a team from a list of games.
-- Input: A list of game identifiers
-- Output: A pair that contains the remaining list of game identifiers
--         and another pair. The second pair contains the selected game and
--         what team is selected to win.
makePick :: [Int] -> RVar ([Int], (Int,Char))
makePick gameIds = 
  do
    (gameIds', gameId) <- selectGame gameIds
    teamId <- flipCoin
    return (gameIds', (gameId,teamId))

makePicks :: Int -> RVar [(Int,Char)]
makePicks n = 
  do
    let ids = [1..n]
    (ids1, g1) <- makePick ids
    (ids2, g2) <- makePick ids1
    (ids3, g3) <- makePick ids2
    (ids4, g4) <- makePick ids3
    (ids5, g5) <- makePick ids4
    return [g1,g2,g3,g4,g5]
