import Control.Applicative
import System.Random
import Data.List

pickNum h = getStdRandom (randomR (1,h))

pickGame :: Int -> IO (Int, Int)
pickGame numGames = pure (\g t -> (g,t)) <*> pickNum numGames <*> pickNum 2

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

-- Need to "adjust" the game number when adding to the list of picks.
-- If a game number already exists in the list of picks, that means
-- that game should be considered "removed" from the list of games that
-- can be picked.
addPick :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
addPick (n,t) picks = let adjust = countIf (\(n',_) -> n' <= n) picks
                      in (n+adjust,t):picks

makePicks :: IO [(Int,Int)]
makePicks = 
  do
    pick1 <- pickGame 16
    pick2 <- pickGame 15
    pick3 <- pickGame 14
    pick4 <- pickGame 13
    pick5 <- pickGame 12
    return (addPick pick5 (addPick pick4 (addPick pick3 (addPick pick2
	(addPick pick1 [])))))
