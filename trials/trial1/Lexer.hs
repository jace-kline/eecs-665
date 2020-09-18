{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Lexer where

import Parser
import ParseRegex
import ParseSpec
import Data.Char
import Text.Regex.Posix
import Text.ParserCombinators.ReadP
import System.IO

type Token = ActionParse
type Line = Int
type Col = Int
type Length = Int
type Location = (Line, Col)
type Input = String

type InputMachine = [ReadP (Token,Length)]

data TokenInstance where
    TokenInstance :: Token -> Location -> TokenInstance

outputTokenInstance :: TokenInstance -> IO ()
outputTokenInstance (TokenInstance Skip (l,c)) = return ()
outputTokenInstance (TokenInstance (Err e) (l,c)) = hPutStrLn stderr $ e ++ concat [" [", show l, show c, "]", "\n"]
outputTokenInstance (TokenInstance (Token n b s) (l,c)) = do
    putStr n
    case b of
        False -> return ()
        True  -> putStr $ ' ':s
    putStr $ concat ["[", show l, show c, "]", "\n"]


regexParsers :: [(RegexParse,Token)] -> InputMachine
regexParsers = map mkRegexParser

mkRegexParser :: (RegexParse,Token) -> ReadP (Token,Length)
mkRegexParser (EOF,t) = eof >> (return (t,1))
mkRegexParser ((RegexParse re),t) = readS_to_P $ \s -> 
    let match = s =~ re :: String
        ret = ((case t of
            (Token n b v) -> (if b then Token n b match else t)
            _             -> t), length match)
    in if null match then [] else [(ret, drop (length match) s)]

matchLongest :: InputMachine -> ReadP (Token,Length)
matchLongest ps = parallel f ps
    where f l@((tl,ll),sl) r@((tr,lr),sr) = if ll >= lr then l else r


lineTokenize :: InputMachine -> ReadP [(Token,Col)]
lineTokenize ps = foldl g start <$> (many $ matchLongest ps)
    where 
        start = [(Skip,1)]
        g :: [(Token,Col)] -> (Token,Length) -> [(Token,Col)]
        g xs@((t,c):_) (t',len) = (t',c+len):xs

lineTokenizer :: InputMachine -> Line -> ReadP [TokenInstance]
lineTokenizer ps l = map f <$> (lineTokenize ps)
    where
        f :: (Token,Col) -> TokenInstance
        f (t,c) = TokenInstance t (l,c)

linesTokenizer :: InputMachine -> [String] -> [TokenInstance]
linesTokenizer ps ls = 
    let lineTokenizers = map (lineTokenizer ps) [1..(length ls)]
        parsedLines = map (uncurry runParser) $ zip lineTokenizers ls
    in concat $ map fst $ concat parsedLines

lexer :: InputMachine -> Input -> IO ()
lexer m s = sequence_ $ map outputTokenInstance $ linesTokenizer m (lines s)