{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Lex where

import Parser
import Token
import Data.Char
import Control.Applicative
import Control.Monad

lexer :: String -> Maybe [Token]
lexer s = 
    let res = runParser lineLex s
        (ts, left) = (fmap fst res, fmap snd res)
    in case left of
        (Just []) -> ts
        _         -> Nothing

lineLex :: Parser Char [Token]
lineLex = many (skipSpaces *> tokenLex <* skipSpaces)

tokenLex :: Parser Char Token
tokenLex = primLex <|> charLitLex <|> intLitLex <|> strLitLex <|> idLex

primLex :: Parser Char Token
primLex = sequenceAlt $ map mkPrimLex allPrims

mkPrimLex :: Prim -> Parser Char Token
mkPrimLex p = string (repr p) >> (return (PrimToken p))


charLitLex :: Parser Char Token
charLitLex = do
    char '\''
    c <- charParser
    return $ CharLitToken c

charParser :: Parser Char Char
charParser = escCharLex <|> satisfy (\c -> isAscii c && isPrint c && c /= '\\' && c /= '\'' && c /= '\"')

escCharLex :: Parser Char Char
escCharLex = do
    char '\\'
    c <- satisfy (\c -> c `elem` escs)
    return $ toEsc c
    where
        escs :: [Char]
        escs = ['t', '\\', 'n', '\'', '\"']
        toEsc :: Char -> Char
        toEsc 't' = '\t'
        toEsc '\\' = '\\'
        toEsc 'n' = '\n'
        toEsc '\'' = '\''
        toEsc '\"' = '\"'

intLitLex :: Parser Char Token
intLitLex = do
    s <- some $ satisfy isDigit
    let i = (read s) :: Int
    return $ IntLitToken i

strLitLex :: Parser Char Token
strLitLex = do
    char '"'
    s <- many charParser
    char '"'
    return $ StrLitToken s

idLex :: Parser Char Token
idLex = do
    let p = \c -> isAlpha c || c == '_'
    c <- satisfy p
    s <- many $ satisfy (\c -> p c || isDigit c)
    return $ IdToken (c:s)