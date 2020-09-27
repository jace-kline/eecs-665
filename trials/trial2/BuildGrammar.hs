module Main where

import Grammar
import SelectorTable
import Data.Binary
import SharedIO
import System.Exit

main :: IO ()
main = do
    contents <- getInFileContents
    case mkLL1Grammar contents of
        Left msg -> putStrLn "Bad grammar" >> exitFailure
        Right t  -> encodeFile "grammar.encoded" t

