module Main where

import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    sequence_ $ map (putStrLn) contents

-- data CharPrimitive = Chr Char
--                     | EscSeq EscCharToken
--                         deriving (Show, Eq)

-- data EscCharToken = Tab | Backslash | Singlequote | Doublequote | Newline | Space
--     deriving (Show, Eq)

-- toEscChar :: String -> Maybe EscCharToken
-- toEscChar s | (s == "\t") = Tab
--             | (s == "\\\"") = Doublequote
--             | (s == "\\'") = Singlequote
--             | (s == "\\_") = Space
--             | (s == "\\n") = Newline
--             | (s == "\\") = Backslash
--             | otherwise = error "No EscCharToken matched"

-- escSeqs :: [String]
-- escSeqs = ["\\t", "\\\"", "\\'", "\\", "\\n", "\\_"]