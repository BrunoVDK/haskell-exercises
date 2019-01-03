module Circuit where

import Data.Maybe
import Data.List (nub,intersperse)

-- Task 1a

data Circuit
  = Input String
  | NOT Circuit
  | AND Circuit Circuit
  | OR Circuit Circuit
  | XOR Circuit Circuit

-- Task 1b

cinput = Input
cnot = NOT
cand = AND
cor = OR
cxor = XOR

-- Task 1c

example = cor (cand (cinput "x") (cinput "y")) (cxor (cnot (cinput "z")) (cinput "x"))

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany (c:cs) = foldr cand c cs

-- Task 2a

instance Show Circuit where
  show (Input v)   = v
  show (NOT c)     = "NOT(" ++ show c ++ ")"
  show (AND c1 c2) = "AND(" ++ show c1 ++ "," ++ show c2 ++ ")"
  show (OR c1 c2) = "OR(" ++ show c1 ++ "," ++ show c2 ++ ")"
  show (XOR c1 c2) = "XOR(" ++ show c1 ++ "," ++ show c2 ++ ")"

-- Task 2b

simplify :: Circuit -> Circuit
simplify (Input c) = Input c
simplify (NOT c)   = NOT (simplify c)
simplify (AND c1 c2) = AND (simplify c1) (simplify c2)
simplify (OR c1 c2)  = NOT (AND (NOT (simplify c1)) (NOT (simplify c2)))
simplify (XOR c1 c2) = simplify (OR (AND c1 (NOT c2)) (AND (NOT c1) c2))

-- Task 2c

size :: Circuit -> Int
size (Input _)   = 0
size (NOT c)     = 1 + size c
size (AND c1 c2) = 1 + size c1 + size c2
size (OR c1 c2)  = 1 + size c1 + size c2
size (XOR c1 c2) = 1 + size c1 + size c2

-- Task 2d

gateDelay :: Circuit -> Int
gateDelay (Input _)   = 0
gateDelay (NOT c)     = 1 + gateDelay c
gateDelay (AND c1 c2) = 1 + max (gateDelay c1) (gateDelay c2)
gateDelay (OR c1 c2) = 1 + max (gateDelay c1) (gateDelay c2)
gateDelay (XOR c1 c2) = 1 + max (gateDelay c1) (gateDelay c2)

-- Task 2e

inputs :: Circuit -> [String]
inputs = nub . go where
  go (Input n)   = [n]
  go (NOT c)     = go c
  go (AND c1 c2) = go c1 ++ go c2
  go (OR c1 c2)  = go c1 ++ go c2
  go (XOR c1 c2) = go c1 ++ go c2

-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (Input n) env = fromJust (lookup n env)
simulate (NOT c) env   = not (simulate c env)
simulate (AND c1 c2) env   = simulate c1 env && simulate c2 env
simulate (OR c1 c2) env   = simulate c1 env || simulate c2 env
simulate (XOR c1 c2) env   = simulate c1 env /= simulate c2 env

-- Task 3b

combinations :: Int -> [[Bool]]
combinations 0 = [[]]
combinations n = let cs = combinations (n-1)
                 in map (False:) cs ++ map (True:) cs

-- Task 3c
tabulate :: Circuit -> IO ()
tabulate c =
  let ns      = inputs c
      header  = concat (intersperse " " ns) ++ " | output"
      line bs = concat (intersperse " " (map toC bs)) ++ " | " ++ toC (simulate c (zip ns bs))
      toC True  = "1"
      toC False = "0"
  in mapM_ putStrLn (header : map line (combinations (length ns))) 
