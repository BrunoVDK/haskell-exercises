module Expense where

import Data.List
import Data.Maybe
import Data.Fixed

-- Expense & Delta

data Expense = MkExpense Double String deriving (Eq, Ord)

mkExpense :: String -> Double -> Expense
mkExpense s d = MkExpense d s

owner :: Expense -> String
owner (MkExpense _ s) = s

amount :: Expense -> Double
amount (MkExpense d _) = d

deltaAmount :: Delta -> Double
deltaAmount = amount . expense

instance Show Expense where
    show (MkExpense d s) = s ++ ": " ++ (show d)

data Delta = MkDelta Expense deriving (Eq, Ord)

expense :: Delta -> Expense
expense (MkDelta e) = e

instance Show Delta where
    show (MkDelta e) = show e

fromExpense :: Double -> Expense -> Delta
fromExpense d (MkExpense de se) = MkDelta (mkExpense se (de - d))

mkDelta name amount = fromExpense 0 (mkExpense name amount)

toDeltas :: [Expense] -> [Delta]
toDeltas es = map (fromExpense av) es
    where av = (sum $ map amount es) / (fromIntegral $ length es)

-- Transferable Transfers

data Transfer = MkTransfer String String Double

instance Show Transfer where
    show (MkTransfer pa pe d) = pa ++ " -> " ++ pe ++ ":" ++ show d

class Transferable t where
    applyTransfer :: Transfer -> t -> t

instance Transferable Expense where
    applyTransfer (MkTransfer pa pe d) (MkExpense ed o)
        | pa == pe || (o /= pa && o /= pe) = mkExpense o ed
        | o == pa = mkExpense o (ed + d)
        | otherwise = mkExpense o (ed - d)

instance Transferable Delta where
    applyTransfer t (MkDelta e) = fromExpense 0 $ applyTransfer t e

createTransfer :: Double -> Delta -> Delta -> Transfer
createTransfer d (MkDelta e1) (MkDelta e2) = MkTransfer (owner e1) (owner e2) d

-- Balancing Expenses

minExpense :: [Expense] -> Expense
minExpense es = snd . minimum $ (map (\e -> (amount e,e)) es)

maxExpense :: [Expense] -> Expense
maxExpense es = snd . maximum $ (map (\e -> (amount e,e)) es)

balanced :: [Expense] -> Double -> Bool
balanced [] eps = True
balanced es eps = abs((amount $ minExpense es) - (amount $ maxExpense es)) < eps

minDelta :: [Delta] -> Delta
minDelta ds = snd . minimum $ (map (\d -> ((deltaAmount d),d)) ds)

maxDelta :: [Delta] -> Delta
maxDelta ds = snd . maximum $ (map (\d -> ((deltaAmount d),d)) ds)

balanceDeltas :: [Delta] -> Double -> [Transfer]
balanceDeltas ds eps = balanceDeltas' ds eps []
balanceDeltas' ds eps ts
    | eps <= e = balanceDeltas' (map (applyTransfer t) ds) eps (t:ts)
    | otherwise = ts
    where   minD = minDelta ds
            maxD = maxDelta ds
            e = abs((deltaAmount minD) - (deltaAmount maxD))
            t = createTransfer (abs $ deltaAmount minD) minD maxD

balance :: [Expense] -> Double -> [Transfer]
balance = balanceDeltas . toDeltas

-- Balancer Application

getExpenses :: IO [Expense]
getExpenses = do
    e <- getExpense
    if (amount e < 0)
        then return []
        else (getExpenses >>= (\es -> return (e:es)))

getExpense :: IO Expense
getExpense = do
    putStr "Name:"
    n <- getLine
    putStr "Amount:"
    a <- (readLn :: IO Double)
    return (mkExpense n a)

printTransfers :: [Transfer] -> IO ()
printTransfers (t:ts) = do
    putStrLn (show t)
    if (length ts > 0)
        then printTransfers ts
        else return ()


