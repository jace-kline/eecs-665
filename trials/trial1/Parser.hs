{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Data.Monoid
import Data.Char
import Text.ParserCombinators.ReadP

data ParseContext where
    TopLevel :: ParseContext
    QuotedStr :: ParseContext
    CharClass :: ParseContext
    QuotedCharClass :: ParseContext
    deriving (Eq, Show)

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

-- These are chars that should not be parsed by the
-- normalSeqParse parser, depending on context
reserveChars :: ParseContext -> [Char]
reserveChars context = case context of
    TopLevel -> "\\<>[()].$*+?|\n\t "
    QuotedStr -> "\""
    CharClass -> "]^"
    QuotedCharClass -> "\""

escSeqParse :: ParseContext -> ReadP String
escSeqParse context = esc +++ space
    where esc = do
                s <- choice $ map string escSeqs
                return [toEscRep s]
          space = do
                string "\\_"
                return $ case context of
                    TopLevel -> "[ ]"
                    _        -> "\\_"

-- parses a character (normal or escape)
singleCharParse :: ParseContext -> ReadP String
singleCharParse context = escSeqParse context <++ p
    where p = do
            ch <- satisfy (\c -> isPrint c && (not $ c `elem` (reserveChars context)))
            return [ch]

infixl 6 <**>
(<**>) :: (Monoid a) => ReadP a -> ReadP a -> ReadP a
l <**> r = do
    lval <- l
    rval <- r
    return $ lval <> rval

trimSpaces :: ReadP a -> ReadP a
trimSpaces p = do
    skipSpaces
    v <- p
    return v

combine :: (Monoid a) => ReadP [a] -> ReadP a
combine pas = foldr1 (<>) <$> pas

surroundedBy :: String -> String -> ReadP String -> ReadP String
surroundedBy l r p = (string l >>= return) <**> p <**> (string r >>= return)

leftAssocOp :: String -> ReadP String -> ReadP String
leftAssocOp op baseparser = f
    where g = baseparser <**> (string op >>= return) <**> f
          f = g <++ baseparser

quotedStr :: ReadP String
quotedStr = surroundedBy "\"" "\"" $ combine $ many $ singleCharParse QuotedStr

parallel :: ((a,String) -> (a,String) -> (a,String)) -> [ReadP a] -> ReadP a
parallel f ps = readS_to_P $ \s ->
    let ys = filter (\xs -> not (null xs)) $ map (\p -> runParser p s) ps
    in case ys of
        [] -> []
        zs -> let (v, rest) = foldr1 f $ concat zs
              in [(v, rest)]

runReadP :: ReadP a -> String -> [(a, String)]
runReadP = readP_to_S

runParser = runReadP

evalReadP :: ReadP a -> String -> Maybe a
evalReadP p s = case runReadP p s of
    []  -> Nothing
    xs  -> Just $ (fst . last) xs

evalParser = evalReadP