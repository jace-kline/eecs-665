{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Regex where

import Data.Char
import Text.Regex.Posix
import Text.ParserCombinators.ReadP

-- Use the operator <string> =~ <regex-string> :: String
-- If no match, an empty string is returned
-- If match, the string is returned

-- Since we imported a library that can directly read strings as Regexs
-- and interpret them, we can just manipulate our strings to match the
-- expected syntax
-- We must make sure that each regex only matches from the front of the string

{- Character classes:

1. Any "\_" should be converted to " " (space character)
2. 

-}

re = "[\\][0-9]*"

regexNormalParse :: ReadP String
regexNormalParse = choice []

escSeqs :: [String]
escSeqs = ["\\t", "\\\"", "\\'", "\\\\", "\\n"]

toEscRep :: String -> Char
toEscRep s = case s of
    "\\t"   -> '\t'
    "\\\""  -> '\"'
    "\\'"   -> '\''
    "\\\\"  -> '\\'
    "\\n"   -> '\n'
    _       -> error $ "Invalid argument"

data ParseContext = TopLevel | QuotedStr | CharClass | QuotedCharClass

-- These are chars that should not be parsed by the
-- normalSeqParse parser, depending on context
reserveChars :: ParseContext -> [Char]
reserveChars context = case context of
    TopLevel -> "\\<>[()].$*+?|\n\t"
    QuotedStr -> "\""
    CharClass -> "]"
    QuotedCharClass -> "\""

-- only applicable at the top level
-- parses <<EOF>>, . , $
specialParse :: ReadP String
specialParse = eofParse +++ dotParse +++ dollarParse
    where
        eofParse = string "<<EOF>>"
        dotParse = string "."
        dollarParse = string "$"

escSeqParse :: ParseContext -> ReadP String
escSeqParse context = many p 
    where p = do
        s <- choice $ map string escSeqs
        return [toEscRep s] +++ do
            string "\\_"
            return $ case context of
                TopLevel -> "[ ]"
                _        -> " "

-- normalSeqParse :: ParseContext -> ReadP String
-- normalSeqParse context = many p
--     where p = do


-- seqParse :: ParseContext -> ReadP String
-- seqParse context = case context of
--     TopLevel -> specialParse <++ escSeqParse context <++ normalSeqParse context
    

runReadP :: ReadP a -> String -> [(a, String)]
runReadP = readP_to_S

main = do
    contents <- readFile "test2.txt"
    sequence_ $ map print contents
    let parsed = runReadP parser contents
    print parsed
    where parser = escParse TopLevel