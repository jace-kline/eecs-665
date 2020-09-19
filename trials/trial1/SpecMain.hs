{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Lex
import SharedIO
import Parser
import ParseSpec
import ParseRegex (RegexParse)
import System.IO
import Data.Binary

main :: IO ()
main = do
    contents <- getInFileContents
    let lineParses = specLinesLexer $ lines contents
    showLineErrors lineParses
    encodeFile "spec.encoded" (concat lineParses)


showLineErrors :: [[(RegexParse, ActionParse)]] -> IO ()
showLineErrors xs = sequence_ $ map f $ zip [1..] xs
    where f (l,v) = case v of
            [] -> hPutStrLn stderr $ "Parse error in spec file on line " ++ show l
            _ -> return ()

specinput = "dog NOUN true\nbites VERB false\nm(a)+n NOUN false\n[ ] (SKIP)\n. (ERR) \"bad input\""