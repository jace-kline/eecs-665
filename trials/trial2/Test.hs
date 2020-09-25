module Test where

import System.IO
import Grammar
import Parser

main = do
    input <- readFile "test.grammar"
    print $ mkGrammar input
    -- let ls = lines input
    -- --print $ extract $ runParser lineParser (head ls)
    -- let lexlines = parseGrammarLines input
    -- print $ (lexlines >>= grpDerivations)
