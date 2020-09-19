{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module ParseSpec where

import Parser
import ParseRegex
import Data.Char
import Text.Regex.Posix
import Text.ParserCombinators.ReadP
import Data.Binary
import GHC.Generics (Generic)

type RuleParse = (RegexParse, ActionParse)
type Name = String
type Value = String

data ActionParse where
    Skip :: ActionParse
    Err :: String -> ActionParse
    Token :: Name -> Bool -> Value -> ActionParse
    deriving (Generic, Eq, Show)

instance Binary ActionParse

specLinesLexer :: [String] -> [[(RegexParse,ActionParse)]]
specLinesLexer ls = map (map fst . runParser ruleParse) ls

ruleParse :: ReadP (RegexParse,ActionParse)
ruleParse = (,) <$> regexParse <*> actionParse

actionParse :: ReadP ActionParse
actionParse = skipSpaces >> (skip +++ err +++ token) >>= \ret -> skipSpaces >> return ret
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
            return $ Token id b []
        token_id = (foldr (:) []) <$> (many1 $ satisfy isUpper)
        store = (string "true" >> return True) +++ (string "false" >> return False)