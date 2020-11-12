{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser t a = Parser { runParser :: [t] -> Maybe (a, [t]) }

instance Functor (Parser t) where
    fmap f (Parser g) = Parser $ \ts -> fmap (\(x,y) -> (f x, y)) $ g ts

instance Applicative (Parser t) where
    pure x = Parser $ \ts -> pure (x, ts)
    liftA2 f (Parser g) (Parser h) = Parser $ \ts -> do
        (x, ts')  <- g ts
        (y, ts'') <- h ts'
        return (f x y, ts'')

instance Alternative (Parser t) where
    empty = Parser $ \_ -> Nothing
    (Parser g) <|> (Parser h) = Parser $ \ts -> 
        case g ts of
            Nothing  -> h ts
            x        -> x

instance Monad (Parser t) where
    return = pure
    (Parser g) >>= h = Parser $ \ts -> 
        case g ts of
            Just (x, ts') -> (runParser $ h x) ts'
            Nothing       -> Nothing

sequenceAlt :: (Alternative f) => [f a] -> f a
sequenceAlt xs = foldr1 (<|>) xs

-- end :: Parser t [a]
-- end = do
--     xs <- look
--     case xs of
--         [] -> return []
--         _  -> empty

-- manyStop :: Parser t a -> Parser t [a]
-- manyStop p = end <|> p'
--     where p' = do
--             x <- p
--             xs <- manyStop p
--             return (x:xs)

-- consumes one token, regardless of value
get :: Parser t t
get = Parser $ \ts -> case ts of
    [] -> Nothing
    (x:xs) -> return (x,xs)

-- returns rest of tokens, consumes none
look :: Parser t [t]
look = Parser $ \ts -> return (ts, ts)

-- terminate when empty tokens list encountered

-- parser to match a single input token
token :: (Eq t) => t -> Parser t t
token t = do
    t' <- get
    if t == t' then return t else empty

char :: Char -> Parser Char Char
char = token

tokens :: (Eq t) => [t] -> Parser t [t]
tokens ts = sequence_ (map token ts) >> return ts

string :: String -> Parser Char String
string = tokens

satisfy :: (t -> Bool) -> Parser t t
satisfy p = do
    t <- get
    if p t then return t else empty

skipSpaces :: Parser Char ()
skipSpaces = (many (satisfy isSpace)) >> return ()