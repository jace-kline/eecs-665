{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Token
import Parser
import Input
import Lex
import Parse

import System.IO
import Control.Monad
import Control.Monad.Fail
import Control.Applicative

parser = unitExpParser

main :: IO ()
main = do
    s <- getInput
    test parser s

testInput :: (Show a) => TokenParser a -> IO ()
testInput p = do
    s <- getInput
    test p s

test :: (Show a) => TokenParser a -> String -> IO ()
test p s = do
    let m_ts = lexer s
    print m_ts
    case m_ts of
        Just ts -> print $ runParser p ts
        _ -> putStrLn "Lexical analysis failed"