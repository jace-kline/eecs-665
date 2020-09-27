{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Data.Monoid
import Data.Char
import Text.ParserCombinators.ReadP

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
    skipSpaces
    return v

combine :: (Monoid a) => ReadP [a] -> ReadP a
combine pas = foldr1 (<>) <$> pas

ignore :: ReadP a -> ReadP ()
ignore p = p >> return ()

surroundedBy :: String -> String -> ReadP String -> ReadP String
surroundedBy l r p = (string l >>= return) <**> p <**> (string r >>= return)

leftAssocOp :: String -> ReadP String -> ReadP String
leftAssocOp op baseparser = f
    where g = baseparser <**> (string op >>= return) <**> f
          f = g <++ baseparser

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

extract :: [(a,b)] -> a
extract = fst . last