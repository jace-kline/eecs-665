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
import IR
import MIPS
import ToIR
import ToMIPS

import System.IO
import System.Environment
import System.Exit
import Data.List (intercalate)
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
main = do
    args <- getArgs
    if length args < 3
        then invalidFormatMsg 
        else do
            let fname = args !! 0
            let flag = args !! 1
            let asm = args !! 2
            if flag == "-m"
            then writeAsm fname asm
            else invalidFormatMsg

writeAsm :: FilePath -> FilePath -> IO ()
writeAsm holeyc asm = do
    s <- readFile holeyc
    case runToolchain s of
        Left errmsg -> putStrLn errmsg
        Right xs -> do
            let output = intercalate "\n" $ map show xs
            writeFile asm output

invalidFormatMsg :: IO ()
invalidFormatMsg = do
    putStrLn "Incorrect Format!"
    putStrLn "Format: ./holeycc <file.holeyc> -m <file.s>"
    exitFailure

testParseProg :: Input -> Either ErrMsg [Effect]
testParseProg s = tryLex s >>= tryParseProg

testToolchainIO :: FilePath -> IO ()
testToolchainIO file = do
    s <- readFile file
    case runToolchain s of
        Left errmsg -> putStrLn errmsg
        Right x     -> sequence_ $ map (putStrLn . show) x

runToolchain :: Input -> Either ErrMsg [MIPSLine]
runToolchain s = do
    ts <- tryLex s
    effs <- tryParseProg ts
    scope <- tryAnalysis effs
    irProg <- pure $ progIR (gblTypes scope) (funcs scope)
    mips <- pure $ toMIPS irProg
    return $ mips

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

tryParseProg :: [Token] -> Either ErrMsg [Effect]
tryParseProg = tryInterpStep parseProg "Parser Error"

tryAnalysis :: [Effect] -> Either ErrMsg Scope
tryAnalysis = tryInterpStep ((flip progAnalyzer) initScope) "Name/Type Analysis Error"

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just x) = Right x