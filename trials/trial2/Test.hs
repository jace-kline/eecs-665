module Test where

import System.IO
import Grammar
import Parser
import FirstFollow

main = do
    input <- readFile "tests/test3.grammar"
    print $ mkLL1Grammar input >>= \g -> return $ (firstSets g, followSets g)
    -- let ls = lines input
    -- --print $ extract $ runParser lineParser (head ls)
    -- let lexlines = parseGrammarLines input
    -- print $ (lexlines >>= grpDerivations)
