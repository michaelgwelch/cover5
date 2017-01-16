{-# LANGUAGE CPP #-}
module Cover5(choose5) where

import Data.Random.Extras(choiceExtract)
import Data.Random(RVar)
import Data.Maybe
import Data.List
import Control.Monad.State(StateT, evalStateT, get, put, lift)
import Control.Monad(replicateM)
import Cover5.Internals

-- The follow two imports are not needed
-- in ghc 8.0 which I wrote this in
-- However travis ci is still at 7.8 and therfore
-- needs them. Also the CPP option at top of file
-- is just for this #if
#if __GLASGOW_HASKELL__ <= 708

import Data.Functor ((<$>))
#endif


-- | Takes as input the number of games in a given week
--   and returns a random list of 5 game identifiers and
--   team identifiers
choose5 :: Int -> RVar [(GameId,TeamId)]
choose5 numGames = sort <$> evalStateT makePicks [1..numGames]

