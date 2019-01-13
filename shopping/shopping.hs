module Shopping where

import Data.List
import Data.Maybe
import Data.Fixed

-- Shopping

data Section = Section Int Int Int deriving (Show)

data Direction = N | Z | B deriving (Show)

helpJan :: [Section] -> [(Direction,Int)]
helpJan xs = reverse . snd $ kortste (0,[]) (0,[]) xs
    where   kortste n z [] = minpad n z
            kortste (n,accN) (z,accZ) (Section u l b:xs) =
                kortste (minpad (n+u,(N,u):accN) (z+l+b,(B,b):(Z,l):accZ))
                        (minpad (n+u+b,(B,b):(N,u):accN) (z+l,(Z,l):accZ))
                        xs
            minpad (n,accN) (z,accZ) = if (n < z) then (n,accN) else (z,accZ)

example = [Section 10 5 3, Section 20 30 2, Section 30 15 3, Section 10 8 0]