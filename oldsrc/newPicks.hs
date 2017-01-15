import Data.Random.Extras
import Data.Random.Show.Unsafe
import Data.Random.RVar
import Data.Maybe
import System.Random
import System.Environment
import Data.List
import Data.Functor


main = 
  do
    args <- getArgs
    let numGames = read $ head args
    print $ sortBy comparePicks <$> makePicks numGames

comparePicks :: (Int,Char) -> (Int,Char) -> Ordering
comparePicks (a,_) (b,_) = compare a b

-- | Makes 5 picks for cover5. Takes an int
--   which is the number of games to choose from.
--   Returns a list of pairs. Each pair indicates
--   a game id and a or b.
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

-- 'Flips' a coin. Returns 'a' or 'b'.
flipCoin :: RVar Char
flipCoin =
  do
    (_,coin) <- fromJust $ choiceExtract "ab"
    return coin


-- Selects a game from a list of game identifiers.
-- Returns the modified list and the chosen game id.
selectGame :: [Int] -> RVar ([Int], Int)
selectGame = fromJust . choiceExtract 

