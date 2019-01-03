myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast ls = reverse ls !! 1

myLength :: [a] -> Int
myLength = sum . (map (const 1))

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
-- You can use flip

isPalindrome :: Eq a => [a] -> Bool
isPalindrome ls = length [f | f <- zipWith (==) ls (reverse ls), f == False] == 0
-- You could have used the function 'and' instead of checking length

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (takeWhile (==x) (x:xs)) : (pack (dropWhile (==x) xs))

encode :: Eq a => [a] -> [(Int,a)]
encode ls = [(length x,head x) | x <- pack ls]

duplic :: [a] -> [a]
duplic = foldr (\x y -> [x,x] ++ y) []

repli :: [a] -> Int -> [a]
repli xs n = [x | x <- xs, i <- [1..n]]

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [xs !! (i-1) | i <- [1..length xs], i `mod` n /= 0]

split :: [a] -> Int -> [[a]]
split xs i = take i xs : [drop i xs]

slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k-i+1) (drop (i-1) xs)

rotate :: [a] -> Int -> [a]
rotate ls i = drop i ls ++ take i ls

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i+1) xs

range :: Int -> Int -> [Int]
range l u   | l <= u = l : (range (succ l) u)
            | otherwise = []

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xs = [[x] | x <- xs]
combinations i (x:xs) = map (x:) (combinations (i-1) xs) ++ (combinations i xs)

split' :: [a] -> [([a],[a])]
split' [] = [([],[])]
split' (x:xs) = ([],x:xs) : [(x:ls,rs) | (ls,rs) <- split' xs ]

intersperse :: Char -> String -> String
intersperse c s = drop 1 $ foldl (\x y -> x ++ [c] ++ [y]) [] s
