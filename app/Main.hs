module Main where

import           Lib
import           System.IO

main :: IO ()
main = do
    contents <- getContents
    let p = prog (filter isToken contents)
    case p of
        Left  err     -> putStrLn err
        Right program -> putStrLn (pprint program)
