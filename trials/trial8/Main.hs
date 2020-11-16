{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Token
import AST
import ProgState

import Parser (runParser)
import Input
import Lex
import Parse
import Analysis
import Eval

import System.IO
import Control.Monad
import Control.Monad.Fail
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Control.Monad.Trans

type ErrMsg = String
type Input = String

main :: IO ()
main = runInterpM initScope interpreter >> succeed

-- testWithInput :: (Show a) => (Input -> Either ErrMsg a) -> IO ()
-- testWithInput f = do
--     s <- getInput
--     let ret = f s
--     case ret of
--         Left msg -> putStrLn msg
--         Right v -> print v

testSteps :: Input -> IO ()
testSteps s = do
    print $ tryLex s
    print $ testParse s
    -- print $ testAnalyze s


interpreter :: InterpM ()
interpreter = interpStep >> interpreter
    where
        interpStep = do
            s <- interpGetInput
            ts <- interpLex s
            effs <- interpParse ts
            b <- interpAnalysis effs
            if b 
                then interpEval effs 
                else return ()

interpGetInput :: InterpM String
interpGetInput = do
    ms <- liftIO getInput
    case ms of
            Nothing -> do
                liftIO $ putStrLn "Error: Bad input"
                return []
            Just s  -> return s

interpLex :: Input -> InterpM [Token]
interpLex s = case lexer s of
    Nothing -> do
        liftIO $ putStrLn "Lexical Error"
        return []
    Just ts -> return ts

interpParse :: [Token] -> InterpM [Effect]
interpParse ts = case parser ts of
    Nothing -> do
        liftIO $ putStrLn "Parse Error"
        return []
    Just effs -> return effs

interpAnalysis :: [Effect] -> InterpM Bool
interpAnalysis effs = do
    s <- get
    let ms = effsAnalyzer effs s
    case ms of
        Nothing -> do
            liftIO $ putStrLn "Name/Type Analysis Error"
            return False
        Just s' -> return True

interpEval :: [Effect] -> InterpM ()
interpEval effs = do
    success <- effsRun effs
    if success
        then succeed
        else liftIO $ putStrLn "Runtime Error"


testParse :: Input -> Either ErrMsg [Effect]
testParse s = tryLex s >>= tryParse

-- testAnalyze :: Input -> Either ErrMsg Scope
-- testAnalyze s = tryLex s >>= tryParse >>= tryAnalyze

testParser :: (Show a) => TokenParser a -> String -> Either String (a,[Token])
testParser p s = tryLex s >>= tryInterpStep (runParser p) "Parse failed"

tryInterpStep :: (Show a, Show b) => (a -> Maybe b) -> ErrMsg -> a -> Either String b
tryInterpStep f msg input = maybeToEither msg $ f input

tryLex :: Input -> Either ErrMsg [Token]
tryLex = tryInterpStep lexer "Lexical Error"

tryParse :: [Token] -> Either ErrMsg [Effect]
tryParse = tryInterpStep parser "Parser Error"

-- tryAnalyze :: [Effect] -> Either ErrMsg Scope
-- tryAnalyze = tryInterpStep (analyzer initScope) "Name/Type Analysis Error"


-- tryAnalysis :: a -> TryM Scope b -> String ->

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just x) = Right x