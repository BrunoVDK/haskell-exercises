module MOPL where

import Data.List
import Data.Maybe

program :: [Statement]
program = [
    assign	"a" (intTerm 8), --a = 8
    printTerm (plus (varTerm "a") (intTerm (-5))), --print(a-5)
    assign "b" (plus (varTerm "a") (intTerm 2)), --b = a+2
    assign "a" (plus (varTerm "a") (varTerm "b")), -- a = a+b
    printTerm (varTerm "a") -- print(a)
    ]

-- Exercise 1

data Term = Variable String | Value Int | Plus Term Term | Times Term Term | Minus Term Term
data Statement = Assignment String Term | Print Term

-- Exercise 2

assign :: String -> Term -> Statement
assign s t = Assignment s t

printTerm :: Term -> Statement
printTerm t = Print t

intTerm :: Int -> Term
intTerm i = Value i

varTerm :: String -> Term
varTerm s = Variable s

plus :: Term -> Term -> Term
plus t1 t2 = Plus t1 t2

times :: Term -> Term -> Term
times t1 t2 = Times t1 t2

minus :: Term -> Term -> Term
minus t1 t2 = Minus t1 t2

-- Exercise 3

valueOf :: [(String,Int)] -> String -> Int
valueOf st s = snd $ head $ filter ((== s) . fst) st

insertS :: String -> Int -> [(String,Int)] -> [(String,Int)]
insertS s i st = (s,i) : (filter ((/= s) .fst) st)

-- Exercise 4

evalTerm :: [(String,Int)] -> Term -> Int
evalTerm st t =
    case t of
        Variable x -> valueOf st x
        Value i -> i
        Plus t1 t2 -> (evalTerm st t1) + (evalTerm st t2)
        Times t1 t2 -> (evalTerm st t1) * (evalTerm st t2)
        Minus t1 t2 -> (evalTerm st t1) - (evalTerm st t2)

-- Exercise 5

execAssign :: String -> Term -> [(String,Int)] -> [(String,Int)]
execAssign s t st = insertS s (evalTerm st t) st

-- Exercise 6

execPure :: [(String,Int)] -> [Statement] -> [(String,Int)]
execPure st [] = st
execPure st (s:ls) =
    case s of
        Print t -> execPure st ls
        Assignment str t -> execPure (execAssign str t st) ls

-- Exercise 7

execute :: [Statement] -> IO ()
execute ls = execute' [] ls
execute' :: [(String,Int)] -> [Statement] -> IO ()
execute' st [] = return ()
execute' st (s:ls) = do
    case s of
        Print t -> (print (evalTerm st t)) >> execute' st ls
        Assignment str t -> execute' (execAssign str t st) ls

