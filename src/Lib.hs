module Lib where

import           Control.Applicative
import           Control.Monad.State
import           Data.List
import           System.IO

data Cmd = Increment | Decrement | Forward | Backward | Print | Get

instance Show Cmd where
    show Increment = "+"
    show Decrement = "-"
    show Forward   = ">"
    show Backward  = "<"
    show Print     = "."
    show Get       = ","

data Stmt = Command Cmd | Loop [Stmt] deriving Show
newtype Prog = Prog [Stmt] deriving Show

isToken :: Char -> Bool
isToken c = c `elem` "+-><.,[]"

fromChar :: Char -> Maybe Cmd
fromChar '+' = Just Increment
fromChar '-' = Just Decrement
fromChar '>' = Just Forward
fromChar '<' = Just Backward
fromChar '.' = Just Print
fromChar ',' = Just Get
fromChar _   = Nothing

cmd :: Char -> Either String Cmd
cmd c = case fromChar c of
    Just cmd -> Right cmd
    Nothing  -> Left ("invalid command" ++ [c])

partitionLoop
    :: String
    -> Either String (String {- chars in loop -}
                            , String) {- chars out of loop -}
partitionLoop []       = Right ([], [])
partitionLoop (c : cs) = case c of
    '[' ->
        let (ins, outs) = partition ((> 0) . snd) (scanl sf ('[', 1) cs)
        in  do
                when (null outs) (Left "missing closing bracket")
                when (any ((/= 0) . snd) outs)
                     (Left "unexpected closing bracket")
                let (ins', outs') = (tail ins, tail outs)
                Right (fmap fst ins', fmap fst outs')
    _ -> Right ([], c : cs)
  where
    sf (_, counter) x = case x of
        '[' -> (x, counter + 1)
        ']' -> (x, counter - 1)
        _   -> (x, counter)

prog :: String -> Either String Prog
prog ""   = Right . Prog $ []
prog body = do
    let (cs, sl) = break (== '[') body
    -- parse commands
    cmds           <- fmap Command <$> mapM cmd cs
    -- parse loop
    (subLoop, cs') <- partitionLoop sl
    let restM = if null subLoop
            then return []
            else do
                cmds'         <- fmap Command <$> mapM cmd cs'
                Prog subLoop' <- prog subLoop
                return (Loop subLoop' : cmds')
    rest <- restM
    Right . Prog $ cmds ++ rest

class PrettyPrint a where
    pprint :: a -> String

instance PrettyPrint Cmd where
    pprint Increment = "++*ptr;"
    pprint Decrement = "--*ptr;"
    pprint Forward   = "++ptr;"
    pprint Backward  = "--ptr;"
    pprint Print     = "putchar(*ptr);"
    pprint Get       = "getchar(ptr);"

instance PrettyPrint Stmt where
    pprint (Command c ) = pprint c
    pprint (Loop    cs) = "while (*ptr) {" ++ (cs >>= pprint) ++ "}"

instance PrettyPrint Prog where
    pprint (Prog sts) =
        "#include <stdio.h>\n"
            ++ "int main() {\n"
            ++ "char arr[30000] = {0}; char *ptr = arr;"
            ++ (sts >>= pprint)
            ++ "    return 0;"
            ++ "}"
