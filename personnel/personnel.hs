module Personnel where

import Data.List
import Data.Maybe
import Text.Read

-- ---------------------------------------------------------------------
-- PERSONNEL
-- ---------------------------------------------------------------------

type EmployeeInput  = (String, Department, Int) -- Name Dep Wage

data Department = ICT | HR | Cleaning
    deriving (Eq, Show)

data Employee = MkICT Int String Int Int
    | MkHR Int String Int Int
    | MkCleaning Int String Int Int

instance Show Employee where
    show (MkICT id na sa wh) = show id ++ " --- " ++ na ++ " --- " ++ show sa ++ " euro/month --- " ++ show wh ++ " hours"
    show (MkHR id na sa co) = show id ++ " --- " ++ na ++ " --- " ++ show sa ++ " euro/month --- " ++ show co ++ " conflicts"
    show (MkCleaning id na sa ro) = show id ++ " --- " ++ na ++ " --- " ++ show sa ++ " euro/month --- " ++ show ro ++ " rooms"

type EmployeeRecord = [Employee]

createEmployee :: EmployeeInput -> Int -> Employee
createEmployee (n,d,w) id = case d of
    ICT -> MkICT id n w 0
    HR -> MkHR id n w 0
    Cleaning -> MkCleaning id n w 0

createInitEmployees :: [EmployeeInput] -> EmployeeRecord
createInitEmployees eis = map (\(ei,id) -> createEmployee ei id) (zip eis [1..length eis])

-- ---------------------------------------------------------------------
-- HIRING
-- ---------------------------------------------------------------------

type Requirement = ( Department -- Department
    , String) --
type Candidate = ( String   -- Name
    , [String] -- Skills
    , Int) -- Wage

getMatchingPercentage :: Department -> [Requirement] -> Candidate -> Int
getMatchingPercentage d rs (n,cs,w) = floor $ 100 * sum [1 | s <- rrs, elem (snd s) cs] / (fromIntegral $ length rrs)
    where rrs = filter ((==d).fst) rs

sortCandidates :: Department -> [Requirement] -> [Candidate] -> [Candidate]
sortCandidates d rs cs = sortBy sortFunc cs
    where   sortFunc x y
                | px == py = compare (wage x) (wage y)
                | otherwise = compare py px
                where   px = getMatchingPercentage d rs x
                        py = getMatchingPercentage d rs y
                        wage (_,_,w) = w

hireCandidate :: Department -> Int -> [Requirement] -> [Candidate] -> Maybe Candidate
hireCandidate d b rs cs
    | 0 == length best = Nothing
    | otherwise = Just (head best)
    where best = dropWhile (\(_,_,w) -> w > b) (sortCandidates d rs cs)

executeHire :: Department -> Int -> [Requirement] -> [Candidate] -> EmployeeRecord -> EmployeeRecord
executeHire d b rs cs = updateRecord (hireCandidate d b rs cs)
    where   updateRecord ca rec = case ca of
                Nothing -> rec
                Just (n,_,w) -> rec ++ [createEmployee (n,d,w) (length rec + 1)]


-- ---------------------------------------------------------------------
-- MAIN
-- ---------------------------------------------------------------------

mainManager :: IO ()
mainManager = mainBase example_requirements example_candidates $ createInitEmployees example_employees

readDepartment :: String -> Maybe Department
readDepartment s
    | s == "ICT" = Just ICT
    | s == "HR" = Just HR
    | s == "Cleaning" = Just Cleaning
    | otherwise = Nothing

prettyPrintEmployeeRecord :: EmployeeRecord -> IO ()
prettyPrintEmployeeRecord es = l >> sequence b >> l >> return ()
    where   b   | 0 == length es = [putStrLn "EMPTY"]
                | otherwise = map (\e -> putStrLn ("| " ++ show e)) es
            l = (putStrLn "+---")

mainBase :: [Requirement] -> [Candidate] -> EmployeeRecord -> IO ()
mainBase rs cs rec = do
    putStrLn "Welcome to the Personnel Register"
    putStrLn "Enter the department where to hire:"
    ds <- getLine
    case (readDepartment ds) of
        Nothing -> do putStrLn "Please try again\n" >> mainBase rs cs rec
        Just d -> do
            putStrLn "Enter the budget:"
            b <- (readLn :: IO Int)
            putStrLn "Personnel register:"
            prettyPrintEmployeeRecord (executeHire d b rs cs rec)


-- ---------------------------------------------------------------------
-- EXAMPLE DATA
-- ---------------------------------------------------------------------

example_employees :: [EmployeeInput]
example_employees = [ ("Tony",  ICT,      5000)
    , ("Bruce", ICT,      2000)
    , ("Nick",  HR,       2000)
    , ("Phil",  HR,       1500)
    , ("Steve", Cleaning, 1500)
    ]

example_requirements :: [Requirement]
example_requirements = [ (ICT,      "Haskell")
    , (ICT,      "Prolog")
    , (ICT,      "Git")
    , (HR,       "PeopleSkills")
    , (HR,       "Connections")
    , (Cleaning, "Experience")
    , (Cleaning, "Motivation")
    ]

example_candidates :: [Candidate]
example_candidates = [ ("Peter",    ["Haskell", "Git", "Motivation"],                                   1000)
    , ("Ben",      ["Haskell", "PeopleSkills", "Connections", "Experience", "Wisdom"], 5000)
    , ("May",      ["PeopleSkills", "Experience", "Motivation"],                       2000)
    , ("MaryJane", ["Prolog", "Connections", "Looks"],                                 1500)
    , ("Harry",    ["Connections", "Motivation", "Money"],                             8000)
    ]

