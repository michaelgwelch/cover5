module Main where

import System.Environment(getArgs)
import Data.Random(sample)
import Cover5(choose5)

main :: IO ()
main = do
         args <- getArgs
         let numGames = if null args then 13 else read $ head args
         putStrLn $ "Making picks for " ++ show numGames ++ " games."
         results <- sample (choose5 numGames) 
         print results


