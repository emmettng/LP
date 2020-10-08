module Main where

import Lib 
import Tribe (Sex(..),PuzzleAnswer(..),sovlePuzzle)

main :: IO ()
main = do 
    putStrLn "------------------Heterosexual Couple---------------"
    mapM_ print (sovlePuzzle (/=))
    putStrLn "------------------Gay Couple---------------"
    mapM_ print (sovlePuzzle (\x y -> x==y && x == Male))
    putStrLn "------------------Lesbin Couple---------------"
    mapM_ print (sovlePuzzle (\x y -> x==y && x == Female))
