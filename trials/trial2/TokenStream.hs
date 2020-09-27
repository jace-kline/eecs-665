module TokenStream where

import Grammar
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Parser

parseTokens :: Grammar -> String -> Maybe [ProdElement]
parseTokens g input = sequence ((map (evalParser (tokenParser g)) (lines input)) ++ [Just Eof])

tokenParser :: Grammar -> ReadP ProdElement
tokenParser g = trimSpaces (termParser (terms g) >>= \term -> lineColParser >> return term)

termParser :: [Terminal] -> ReadP ProdElement
termParser ts = trimSpaces (noVal +++ withVal)
    where
        noVal :: ReadP ProdElement
        noVal = Term <$> (choice $ map string ts)
        withVal :: ReadP ProdElement
        withVal = do
            t <- noVal
            char ':'
            many1 $ satisfy (\c -> isAscii c && not (isSpace c))
            return t

lineColParser :: ReadP ()
lineColParser = trimSpaces $ sequence_ $ intersperse skipSpaces seq
    where 
        seq = [char' '[', digits, char' ',', digits, char' ']']
        digits = ignore (many1 $ satisfy (\c -> isDigit c))
        char' c = ignore $ char c

