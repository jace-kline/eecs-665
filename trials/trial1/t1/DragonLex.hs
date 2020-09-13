module Main where

import System.IO
import System.Environment
import LexerSpec

main = do
    args <- getArgs
    if length args /= 1 
        then usage "Invalid argument count. Quitting program."
        else do
            let fname = last args
            contents <- readFile fname
            parseSpec contents
    
usage :: String -> IO ()
usage str = do
    putStrLn str
    putStrLn "Usage: ./dragonlex <input.spec>"