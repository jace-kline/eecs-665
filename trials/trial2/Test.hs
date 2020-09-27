module Test where

import System.IO
import Grammar
import Parser
import FirstFollow
import SelectorTable
import TokenStream
import Accepter

main = do
    grammar_input <- readFile "tests/test4.grammar"
    token_input <- readFile "tests/test4.tokens"
    --print $ mkLL1Grammar input >>= \g -> return $ (firstSets g, followSets g)
    print $ mkLL1Grammar grammar_input >>= \g -> return $ accepter g token_input
    -- let ls = lines input
    -- --print $ extract $ runParser lineParser (head ls)
    -- let lexlines = parseGrammarLines input
    -- print $ (lexlines >>= grpDerivations)
