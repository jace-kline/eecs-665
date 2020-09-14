module StringParser where

import Parser
import Data.Char

-- infix applicative-like operator for ignoring in-between whitespace
infixl 4 <**>
(<**>) :: Parser Char (a -> b) -> Parser Char a -> Parser Char b
pf <**> p2 = pf <*> (wsParser *> p2)

wsParser :: Parser Char ()
wsParser = Parser $ \ts -> pure ((), trimWS ts)

newlineParser :: Parser Char ()
newlineParser = Parser $ \ts -> pure ((), f ts)
    where f [] = []
          f (t:ts) = if isNewline t then ts else (t:ts) 

trimWS :: String -> String
trimWS = dropWhile isSpaceNotNewline
    where
        isSpaceNotNewline s = isSpace s && not (isNewline s)

isNewline :: Char -> Bool
isNewline '\n' = True
isNewline _    = False