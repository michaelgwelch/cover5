module Main where

import System.Environment(getArgs)
import Data.Random()
import Data.Random.RVar(sampleRVar)
import Cover5(choose5)

main :: IO ()
main = do
         args <- getArgs
         let numGames = if null args then 13 else read $ head args
         putStrLn $ "Making picks for " ++ show numGames ++ " games."
         results <- sampleRVar $ choose5 numGames
         print results


