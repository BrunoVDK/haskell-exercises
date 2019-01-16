module Myhaskell where

import Data.List (sort, sortOn)
import Data.Tuple (swap)
import Data.Ord (Down(..))


-- | A Pokémon is represented by its name.
--
-- Don't worry, when you're writing tests, you don't have to come up with
-- actual Pokémon names, but here are some in case you are uninspired:
-- Pikachu, Charmander, Squirtle, Bulbasaur
type Pokemon = String

-- | A location is a tuple of the latitude and longitude.
type Location = (Float, Float)

data WeekDay
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Ord, Show, Read, Enum)

data Time = Time
            WeekDay
            Int      -- ^ Hours go from 0 to 23
            Int      -- ^ Minutes go from 0 to 59
            deriving (Eq, Ord, Show, Read)

timeHour :: Time -> Int
timeHour (Time _ h _) = h

timeMinutes :: Time -> Int
timeMinutes (Time _ _ m) = m

timeWeekday :: Time -> WeekDay
timeWeekday (Time w _ _) = w

-- | A Pokémon spawned at a location and time.
data Spawn = Spawn Pokemon Location Time
             deriving (Eq, Show, Read)


-- Toy data for the examples
pidgeySpawn1 :: Spawn
pidgeySpawn1 = Spawn "Pidgey" (50.86296,4.674903) (Time Tuesday 12 04)

pidgeySpawn2 :: Spawn
pidgeySpawn2 = Spawn "Pidgey" (50.864605,4.6786203) (Time Friday 3 32)

pikachuSpawn :: Spawn
pikachuSpawn = Spawn "Pikachu" (50.864605,4.6786203) (Time Friday 12 04)

testSpawns :: [Spawn]
testSpawns =  [pidgeySpawn1, pikachuSpawn, pidgeySpawn2]


-- | 1. Projection functions
spawnPokemon :: Spawn -> Pokemon
spawnPokemon (Spawn n _ _) = n

spawnLocation :: Spawn -> Location
spawnLocation (Spawn _ l _) = l

spawnTime :: Spawn -> Time
spawnTime (Spawn _ _ t) = t

-- | 2. Group a list of `Spawn`s by a given function.
groupSpawnsBy :: Eq k => (Spawn -> k) -> [Spawn] -> [(k, [Spawn])]
groupSpawnsBy _ [] = []
groupSpawnsBy f (x:xs) = (fx, group) : groupSpawnsBy f rest
    where   fx = f x
            group = filter ((==) fx . f) (x:xs)
            rest = filter ((/=) fx . f) xs

-- | 3. Which Pokémon spawns most often?
mostCommon :: Eq k => (Spawn -> k) -> [Spawn] -> [(k, Int)]
mostCommon f ls = sortOn (Down . snd) $ map (\(p,l) -> (p, length l)) (groupSpawnsBy f ls)

mostCommonPokemon :: [Spawn] -> [(Pokemon, Int)]
mostCommonPokemon = mostCommon spawnPokemon

-- | 4. At which spawn point does the given Pokémon spawn most often?
topSpawnPointsOf :: Pokemon -> [Spawn] -> [(Location, Int)]
topSpawnPointsOf p ls = mostCommon spawnLocation $ filter ((==p) . spawnPokemon) ls

-- | 5. During which hours do the most Pokémon spawn?
topHours :: [Spawn] -> [(Int, Int)]
topHours = mostCommon (timeHour . spawnTime)

-- | 6. On which day of the week do the most Pokémon spawn?
topWeekDays :: [Spawn] -> [(WeekDay, Int)]
topWeekDays = mostCommon (timeWeekday . spawnTime)

split :: (Spawn -> Bool) -> [Spawn] -> (Int, Int)
split f ls = (nb, (length ls) - nb)
    where nb = length $ filter f ls

-- | 7. How many Pokémon spawn during the day, how many during the night?
dayAndNight :: [Spawn] -> (Int, Int)
dayAndNight = split (day.spawnTime)
    where   day t = (timeHour t >= 7 && timeHour t < 21) || (timeHour t == 21 && timeMinutes t == 0)

-- | 8. How many Pokémon spawn around the hour and how many between the hours?
aroundTheHours :: [Spawn] -> (Int, Int)
aroundTheHours = split (around.spawnTime)
    where   around t = timeMinutes t >= 45 || timeMinutes t < 15
-- Exclusief of inclusief 15? Opgave zegt exclusief, tests doen inclusief

-- | 9. Analyse the spawn data.
analyseSpawns :: IO ()
analyseSpawns = do
    ls <- read <$> (readFile "spawns.data")
    let ps = mostCommonPokemon ls
        mcp = fst $ (ps !! 0)
    putStrLn "# Most common Pokemon:"
    printOrder (\(p,h) -> p ++ " spawned " ++ show h ++ " times") ps
    putStrLn $ "\n# Top spawn points of " ++ mcp ++ ":"
    printOrder (\(l,n) -> "at " ++ show l ++ " " ++ show n ++ " " ++ mcp ++ "s spawned") (topSpawnPointsOf mcp ls)
    putStrLn "\n# Top hours:"
    printOrder (\(h,n) -> "at " ++ show h ++ " o'clock " ++ show n ++ " Pokemon spawned") (topHours ls)
    putStrLn "\n# Top week days:"
    printOrder (\(w,n) -> "on " ++ show w ++ " " ++ show n ++ " Pokemon spawned") (topWeekDays ls)
    let (d,n) = dayAndNight ls
    putStr "\n More Pokemon spawn during the "
    if (d > n)
        then putStr ("day than during the night: " ++ show d ++ " vs " ++ show n)
        else putStr ("night than during the day: " ++ show n ++ " vs" ++ show d)
    let (a,b) = aroundTheHours ls
    putStr "\n More Pokemon spawn "
    if (a > b)
        then putStr ("around the hours than between the hours: " ++ show a ++ " vs " ++ show b)
        else putStr ("between the hours than around the hours: " ++ show b ++ " vs" ++ show a)
    putStrLn ""

printOrder :: (a -> String) -> [a] -> IO [()]
printOrder f rs = sequence . (map putStrLn) $ zipWith (++) ["1. ", "2. ", "3. "] (map f (take 3 rs))




