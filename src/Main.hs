module Main where

import System.Environment
import Cover5

main :: IO ()
main = do
         args <- getArgs
         let numGames = if null args then 13 else (read $ head args)
         putStrLn $ show $ run numGames
