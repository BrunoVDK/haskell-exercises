module MyHaskell where

import Data.List

-- Task 1a

data Circuit
        = Input String 
        | NOT Circuit 
        | AND Circuit Circuit 
        | OR Circuit Circuit
        | XOR Circuit Circuit

-- Task 1b

cinput :: String -> Circuit 
cinput = Input

cnot   :: Circuit -> Circuit
cnot   = NOT

cand   :: Circuit -> Circuit -> Circuit
cand   = AND

cor    :: Circuit -> Circuit -> Circuit
cor    = OR

cxor   :: Circuit -> Circuit -> Circuit
cxor   = XOR

-- Task 1c

example :: Circuit
example = cor (cand x y) (cxor (cnot z) x)
        where   x = cinput "x"
                y = cinput "y"
                z = cinput "z"

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany (c:cs) = foldl cand c cs

-- Task 2a

instance Show Circuit where
  show (Input n) = n
  show (NOT x) = "NOT(" ++ show x ++ ")"
  show (AND x y) = "AND(" ++ show x ++ "," ++ show y ++ ")"
  show (OR x y) = "OR(" ++ show x ++ "," ++ show y ++ ")"
  show (XOR x y) = "XOR(" ++ show x ++ "," ++ show y ++ ")"

-- Task 2b

simplify :: Circuit -> Circuit
simplify (Input n) = cinput n
simplify (NOT x) = cnot (simplify x)
simplify (AND x y) = cand (simplify x) (simplify y)
simplify (OR x y) = cnot (cand (cnot (simplify x)) (cnot (simplify y)))
simplify (XOR x y) = simplify $ cor (cand (simplify x) (cnot (simplify y))) (cand (cnot (simplify x)) (simplify y))

-- Functions below use simplify though model solution doesn't
-- https://en.wikipedia.org/wiki/NAND_logic

-- Task 2c

size :: Circuit -> Int
size = size' . simplify
    where   size' (NOT x) = 1 + size' x
            size' (AND x y) = 1 + (size' x) + (size' y)
            size' _ = 0

-- Task 2d

gateDelay :: Circuit -> Int
gateDelay = gateDelay' . simplify
    where   gateDelay' (NOT x) = 1 + gateDelay' x
            gateDelay' (AND x y) = 1 + maximum (gateDelay' x:[gateDelay' y])
            gateDelay' _ = 0

-- Task 2e

inputs :: Circuit -> [String]
inputs = nub . inputs' . simplify
    where   inputs' (NOT x) = inputs' x
            inputs' (AND x y) = (++) (inputs' x) (inputs' y)
            inputs' (Input n) = [n]

-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (Input n) vs = elem (n,True) vs
simulate (NOT x) vs = not $ simulate x vs
simulate (AND x y) vs = simulate x vs && simulate y vs
simulate (OR x y) vs = simulate x vs || simulate y vs
simulate (XOR x y) vs = (simulate x vs) /= (simulate y vs)

-- Task 3b

combinations :: Int -> [[Bool]]
combinations 0 = [[]]
combinations x = (map (False:) cs) ++ (map (True:) cs)
        where   cs = combinations (x-1)

-- Task 3c

tabulate :: Circuit -> IO ()
tabulate c = foldl (\x y -> x >> csrow y) header (combinations $ length ip)
        where   ip = inputs c
                header = row ip >> putStrLn "output"
                csrow co = (row $ map (show.fromEnum) co) >> putStrLn ((show.fromEnum) (simulate c $ zip ip co))
                row = foldr (\s p -> putStr s >> putStr " " >> p) (putStr "| ")
-- row prints given list of strings, separated by spaces, then prints "| "
-- header uses row to print the first line
-- csrow co uses row to print out output for a given combination
-- then all rows are printed by folding header with csrow for all combinations


  
