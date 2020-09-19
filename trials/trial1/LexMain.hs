{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import ParseRegex (RegexParse)
import Lex (Token, lexer, regexParsers)
import SharedIO
import Data.Binary

main :: IO ()
main = do
    contents <- getInFileContents
    spec <- decodeFile "spec.encoded"
    -- print (spec :: [(RegexParse,Token)])
    let machine = regexParsers spec
    lexer machine contents
