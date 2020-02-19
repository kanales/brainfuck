module BrainFuck where

data Token = Increment | Decrement | Forward | Backward | Print | Get | POpen | PClose deriving Show

tokenize :: String -> [Token]
tokenize []         = []
tokenize ('+' : xs) = Increment : tokenize xs
tokenize ('-' : xs) = Decrement : tokenize xs
tokenize ('<' : xs) = Backward : tokenize xs
tokenize ('>' : xs) = Forward : tokenize xs
tokenize ('.' : xs) = Print : tokenize xs
tokenize (',' : xs) = Get : tokenize xs
tokenize ('[' : xs) = POpen : tokenize xs
tokenize (']' : xs) = PClose : tokenize xs
tokenize (_   : xs) = tokenize xs

data Cmd = Command Token | Loop [Cmd] deriving Show

parse :: [Token] -> [Cmd]
parse ts = case ts of
    [] -> []
    (POpen : rest) ->
        let (inner, rem) = parseLoop ([], rest) in inner : parse rem
    (PClose : _   ) -> error "unexpected closing bracket"
    (x      : rest) -> Command x : parse rest
  where
    parseLoop :: ([Cmd], [Token]) -> (Cmd, [Token])
    parseLoop (prev, ts) = case ts of
        []              -> error "unbalanced brackets"
        (PClose : rest) -> (Loop (reverse prev), rest)
        (POpen : rest) ->
            let (innerLoop, rem) = parseLoop ([], rest)
            in  parseLoop (innerLoop : prev, rem)
        (t : rest) -> parseLoop (Command t : prev, rest)


type Zipper = ([Int], [Int])

forward :: Zipper -> Zipper
forward (xs, []    ) = (0 : xs, [])
forward (xs, y : ys) = (y : xs, ys)

backward :: Zipper -> Zipper
backward ([]    , ys) = ([], 0 : ys)
backward (x : xs, ys) = (xs, x : ys)

get :: Zipper -> Int
get (_, []   ) = 0
get (_, y : _) = y

set :: Zipper -> Int -> Zipper
set (xs, []    ) y = (xs, [y])
set (xs, _ : ys) y = (xs, y : ys)

eval :: Zipper -> [Cmd] -> IO ()
eval z cmd = case cmd of
    (Command Backward  : rest) -> eval (backward z) rest
    (Command Forward   : rest) -> eval (forward z) rest
    (Command Increment : rest) -> eval (set z $ get z + 1) rest
    (Command Decrement : rest) -> eval (set z $ get z - 1) rest
    (Command Print     : rest) -> print (get z) >> eval z rest
    (Command Get       : rest) -> do
        x <- readLn
        eval (set z x) rest
    [] -> return ()

