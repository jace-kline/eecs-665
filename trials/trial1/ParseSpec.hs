{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module ParseSpec where

import Parser
import ParseRegex
import Data.Char
import Text.Regex.Posix
import Text.ParserCombinators.ReadP

data RuleParse where
    RuleParse :: RegexParse -> ActionParse -> RuleParse

data ActionParse where
    Skip :: ActionParse
    Err :: String -> ActionParse
    Token :: String -> Bool -> ActionParse
    deriving (Eq, Show)

actionParse :: ReadP ActionParse
actionParse = skipSpaces >> (skip +++ err +++ token)
    where 
        skip = string "(SKIP)" >> return Skip
        err = do
            string "(ERR)"
            skipSpaces
            e <- quotedStr
            return $ Err e
        token = do
            id <- token_id
            skipSpaces
            b <- store
            return $ Token id b
        token_id = (foldr (:) []) <$> (many1 $ satisfy isUpper)
        store = (string "true" >> return True) +++ (string "false" >> return False)