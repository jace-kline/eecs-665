{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module ParseRegex where

import Parser
import Data.Char
import Text.Regex.Posix
import Text.ParserCombinators.ReadP
import Data.Binary
import GHC.Generics (Generic)

data RegexParse where
    EOF :: RegexParse
    RegexParse :: String -> RegexParse
    deriving (Generic, Eq, Show)

instance Binary RegexParse

regexParse :: ReadP RegexParse
regexParse = skipSpaces >> (eof <++ regex)
    where 
        eof = string "<<EOF>>" >> return EOF
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
groupParse = parens <++ quotedParse <++ dotParse <++ charParse
    where
        dotParse = string "." >>= return 
        parens = surroundedBy "(" ")" disjunctionsParse

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