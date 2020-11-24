{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Input where

import Data.Char
import System.IO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

{-
How to get input from user:
1. read a line
2. scan line to see if curly braces match
    - store the nesting depth if dont match
3. if lcurly > rcurly, continue input to next line
    else if rcurly > lcurly, error
    else return the current string
-}

getInput :: IO (Maybe String)
getInput = runMaybeT getInputM

getInputM :: MaybeT IO String
getInputM = go 0
    where
        go :: Int -> MaybeT IO String
        go n = do
            s <- liftIO getLine
            let n' = countBraces s 0
            let diff = n + n'
            if diff < 0 
                then mzero
                else if diff > 0 
                        then do
                            liftIO $ do
                                putStr $ (take diff (repeat '.')) ++ " "
                                hFlush stdout
                            s' <- go diff
                            return $ s ++ s'
                        else return s

countBraces :: String -> Int -> Int
countBraces [] x = x
countBraces (c:cs) x | (c == '{') = countBraces cs (x + 1)
                     | (c == '}') = countBraces cs (x - 1)
                     | otherwise = countBraces cs x