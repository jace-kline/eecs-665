module Test where

import System.IO
import Grammar
import Parser

main = do
    input <- readFile "tests/test2.grammar"
    print $ mkLL1Grammar input
    -- let ls = lines input
    -- --print $ extract $ runParser lineParser (head ls)
    -- let lexlines = parseGrammarLines input
    -- print $ (lexlines >>= grpDerivations)
