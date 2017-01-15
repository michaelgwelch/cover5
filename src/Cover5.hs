{-# LANGUAGE CPP #-}
module Cover5(choose5) where

import Data.Random.Extras(choiceExtract)
import Data.Random(RVar)
import Data.Maybe
import Data.List
import Control.Monad.State(StateT, evalStateT, get, put, lift)

-- The follow two imports are not needed
-- in ghc 8.0 which I wrote this in
-- However travis ci is still at 7.8 and therfore
-- needs them. Also the CPP option at top of file
-- is just for this #if
#if __GLASGOW_HASKELL__ <= 708
import Data.Functor (<$>)
import Control.Applicative (<*>)
#endif

choose5 :: Int -> RVar [(Int,Char)]
choose5 numGames = sortBy comparePicks <$> 
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
