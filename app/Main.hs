module Main where

import           BrainFuck
import           System.Environment
import           System.IO

main :: IO ()
main = do
    [name] <- getArgs
    withFile name ReadMode $ \fp -> do
        input <- hGetContents fp
        (eval empty . parse . tokenize) input
        return ()
