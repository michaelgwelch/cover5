module Cover5(choose5) where

import Data.Random.Extras(choiceExtract)
import Data.Random(RVar)
import Data.Maybe
import Data.List
import Control.Monad.State(StateT, evalStateT, get, put, lift)

choose5 :: Int -> RVar [(Int,Char)]
choose5 numGames = sort <$> evalStateT makePicks [1..numGames]


-- | Makes 5 picks for cover5. Takes an int
--   which is the number of games to choose from.
--   Returns a list of pairs. Each pair indicates
--   a game id and a or b.
makePicks :: StateT [Int] RVar [(Int,Char)]
makePicks = sequence $ replicate 5 makePick

-- A state transformer that selects a game and a team from a list of games.
-- The state is modified to remove the selected game from the state.
makePick :: StateT [Int] RVar (Int,Char)
makePick = (,) <$> selectGame <*> lift flipCoin
-- alternatively, using the monad
-- makePick = do
--              gameId <- selectGame
--              teamId <- lift flipCoin
--              return (gameId,teamId)


-- 'Flips' a coin. Returns 'a' or 'b'.
flipCoin :: RVar Char
flipCoin = snd <$> fromJust (choiceExtract "ab")
-- alternatively, using the monad:
-- flipCoin = do
--              (_,coin) <- fromJust $ choiceExtract "ab"
--              return coin

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
