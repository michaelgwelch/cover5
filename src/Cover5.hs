module Cover5(choose5) where

import Data.Random.Extras(choiceExtract)
import Data.Random(RVar)
import Data.Maybe
import Data.List
import Control.Monad.State(StateT, evalStateT, get, put, lift)
import Control.Monad(replicateM)

type GameId = Int
type TeamId = Char

choose5 :: Int -> RVar [(GameId,TeamId)]
choose5 numGames = sort <$> evalStateT makePicks [1..numGames]


-- | Makes 5 picks for cover5. Takes an int
--   which is the number of games to choose from.
--   Returns a list of pairs. Each pair indicates
--   a game id and a or b.
makePicks :: StateT [GameId] RVar [(GameId,TeamId)]
makePicks = replicateM 5 makePick

-- A state transformer that selects a game and a team from a list of games.
-- The state is modified to remove the selected game from the state.
makePick :: StateT [GameId] RVar (GameId,TeamId)
makePick = (,) <$> selectGame <*> lift flipCoin

-- 'Flips' a coin. Returns 'a' or 'b'.
flipCoin :: RVar TeamId
flipCoin = snd <$> fromJust (choiceExtract "ab")

-- A state transformer that selects a game identifier 
-- from the list of identifiers
-- stored in the state. The state is modified to remove
-- the chosen identifier.
selectGame :: StateT [GameId] RVar GameId
selectGame = do
                gs <- get
                (gs',g) <- lift $ fromJust $ choiceExtract gs
                put gs'
                return g
