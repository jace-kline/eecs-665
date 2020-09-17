{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module ParseRegex where

import Parser
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

data RegexParse where
    EOF :: RegexParse
    RegexParse :: String -> RegexParse
    deriving (Eq, Show)

regexParse :: ReadP RegexParse
regexParse = skipSpaces >> (eof <++ regex)
    where 
        eof = string "<<EOF>>" >> skipSpaces >> return EOF
        regex = RegexParse <$> ((return "^") <**> disjunctionsParse)
        
disjunctionsParse :: ReadP String
disjunctionsParse = leftAssocOp "|" conjunctionsParse

conjunctionsParse :: ReadP String
conjunctionsParse = eol <++ p <**> eol <++ p
    where
        p = leftAssocOp "" unaryBunchParse
        eol = string "$" >>= return

unaryBunchParse :: ReadP String
unaryBunchParse = groupParse <**> opParse <++ groupParse
    where
        opParse = choice $ map f ["*", "+", "?"]
        f op = string op >>= return

groupParse :: ReadP String
groupParse = parens <++ quotedParse <++ charParse
    where parens = skipSpaces >> surroundedBy "(" ")" disjunctionsParse

quotedParse :: ReadP String
quotedParse = surroundedBy "\"" "\"" $ combine $ many1 $ singleCharParse QuotedStr

-- parses a single character from either normal characters, escape characters, or parses a character class
charParse :: ReadP String
charParse = charClassParse +++ singleCharParse TopLevel

charClassParse :: ReadP String
charClassParse = surroundedBy "[" "]" p
    where 
        p = notted <++ combine (many1 normal)
        notted = (string "^" >>= return) <**> (combine (many1 normal))
        normal = quoted <++ range <++ singleCharParse CharClass
        quoted = surroundedBy "\"" "\"" $ singleCharParse QuotedCharClass
        range = do
            l <- satisfy isAlphaNum
            let f = \c -> ((isAlpha c && isAlpha c) || (isDigit c && isDigit l)) && (ord c > ord l)
            dash <- string "-"
            r <- satisfy f
            return $ l:dash ++ [r]    

parseRegex :: String -> [(RegexParse, String)]
parseRegex = runReadP regexParse

main = do
    contents <- readFile "test2.txt"
    sequence_ $ map print contents
    let parsed = runReadP parser contents
    print parsed
    where parser = escSeqParse TopLevel