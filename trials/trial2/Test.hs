module Test where

import System.IO
import Grammar
import Parser
import FirstFollow
import SelectorTable
import TokenStream
import Accepter

main = do
    grammar_input <- readFile "test.grammar"
    --token_input <- readFile "tests/test4.tokens"
    let maybe_g = mkGrammar grammar_input
    case maybe_g of
        Nothing -> putStrLn "Bad grammar"
        Just g  -> putStr $ "FIRST sets: " ++ show (firstSets g) ++ "\nFOLLOW sets: " ++ show (followSets g) ++ "\n"
    --print $ mkLL1Grammar grammar_input >>= \g -> return $ accepter g token_input
    -- let ls = lines input
    -- --print $ extract $ runParser lineParser (head ls)
    -- let lexlines = parseGrammarLines input
    -- print $ (lexlines >>= grpDerivations)
