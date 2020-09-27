module SharedIO where

import System.IO
import System.Environment
import System.Directory
import System.Exit

getInFileContents :: IO String
getInFileContents = do
    fname <- getInFileArg
    contents <- readFile fname
    return contents

getInFileArg :: IO String
getInFileArg = do
    args <- getArgs
    if length args /= 1 
        then usage
        else do
            let fname = head args
            b <- doesFileExist fname
            if b then return fname
            else do
                putStrLn $ "'" ++ fname ++ "' is not a valid file."
                exitFailure

usage = do
    progName <- getProgName
    putStrLn $ "Error: Invalid argument count"
    putStrLn $ "Usage: ./" ++ progName ++ " <input-file>"
    exitFailure