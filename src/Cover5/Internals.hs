{-# LANGUAGE CPP #-}
module Cover5.Internals
  (
    TeamId(TeamA,TeamB), 
    GameId, 
    makePicks, 
    makePick,
    flipCoin,
    selectGame
  ) where

import Data.Random.Extras(choice,choiceExtract)
import Data.Random(RVar)
import Data.Maybe
import Data.List
import Control.Monad.State(StateT, evalStateT, get, put, lift)
import Control.Monad(replicateM)

-- The follow two imports are not needed
-- in ghc 8.0 which I wrote this in
-- However travis ci is still at 7.8 and therfore
-- needs them. Also the CPP option at top of file
-- is just for this #if
#if __GLASGOW_HASKELL__ <= 708

import Data.Functor ((<$>))
import Control.Applicative ((<*>))
#endif

type GameId = Int
data TeamId = TeamA | TeamB deriving (Eq, Ord, Show)

-- | Makes 5 picks for cover5. Takes an int
--   which is the number of games to choose from.
--   Returns a list of pairs. Each pair indicates
--   a game id and TeamA or TeamB.
makePicks :: StateT [GameId] RVar [(GameId,TeamId)]
makePicks = replicateM 5 makePick

-- | A state transformer that randomly selects a game and a team from a list of
--   games.  The state is modified to remove the selected game from the state.
makePick :: StateT [GameId] RVar (GameId,TeamId)
makePick = (,) <$> selectGame <*> lift flipCoin

-- | 'Flips' a coin to pick TeamA or TeamB. 
flipCoin :: RVar TeamId
flipCoin = choice [TeamA,TeamB]

-- | A state transformer that randomly selects a game identifier 
--   from the list of identifiers stored in state. The state is modified 
--   to remove the chosen identifier.
selectGame :: StateT [GameId] RVar GameId
selectGame = do
                gs <- get
                (gs',g) <- lift $ fromJust $ choiceExtract gs
                put gs'
                return g
