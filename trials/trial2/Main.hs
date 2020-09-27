module Main where

import Grammar
import Accepter
import Data.Binary
import SharedIO
import System.Exit

main :: IO ()
main = do
    contents <- getInFileContents
    g <- decodeFile "grammar.encoded"
    case accepter g contents of
        False -> putStrLn "rejected" >> exitFailure
        True  -> putStrLn "accepted"