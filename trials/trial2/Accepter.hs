module Accepter where

import Grammar
import TokenStream
import SelectorTable

type Stack = [ProdElement]
type Lookahead = ProdElement

accepter :: Grammar -> String -> Bool
accepter g input = case tokens of
    Nothing -> False
    Just ts -> go [Var (start g), Eof] ts
    where
        table = selectorTable g
        tokens = parseTokens g input
        go :: Stack -> [Lookahead] -> Bool
        go [] [] = True
        go [] ts = False
        go (x:xs) (t:ts) = case x of
            (Var v) -> case lookup (x,t) table of
                Just (Prod ys) -> go (ys ++ xs) (t:ts)
                Just Epsilon   -> go xs (t:ts)
                Nothing        -> False
            _       -> if t == x then go xs ts else False