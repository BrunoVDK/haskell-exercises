module Phonebook (
  Name,
  PhoneNumber,
  Entry, mkEntry,name,phone,
  PhoneBook, names, phones, owner,
  Index(findEntry,empty,singleton,(<+>)),
  Assoc,
  byName,byPhone, emptyBook, addToBook, fromEntries,
  number, callerID,
  bill,bob,jeb,val,
  billbook,bobbook,jebbook,valbook,
  Lookup)
where  

import Data.List (intercalate)

-- Entry

type Name        = String
type PhoneNumber = [Int]

showPhone :: PhoneNumber -> String
showPhone = intercalate " " . map show

data Entry
  = MkEntry Name PhoneNumber
  deriving (Eq,Show)

mkEntry :: Name -> PhoneNumber -> Entry
mkEntry n p = MkEntry n p

name :: Entry -> Name
name (MkEntry n _) = n

phone :: Entry -> PhoneNumber
phone (MkEntry _ p) = p

-- Index

class Index i where
  findEntry :: Eq k => k -> i k -> Maybe Entry
  empty     :: Eq k => i k
  singleton :: Eq k => k -> Entry -> i k
  (<+>)     :: Eq k => i k -> i k -> i k

data Assoc k
  = MkAssoc [(k,Entry)]
  deriving (Eq,Show)

instance Index Assoc where
    findEntry k (MkAssoc a) = if (length es > 0) then Just (snd $ head es) else Nothing
        where es = filter ((==k).fst) a
    empty = MkAssoc []
    singleton k e = MkAssoc [(k,e)]
    (<+>) (MkAssoc a) (MkAssoc b) = MkAssoc (a ++ (filter (\e -> findEntry (fst e) (MkAssoc a) == Nothing) b))

-- Phonebook

data PhoneBook  = MkPhoneBook Entry (Assoc Name) (Assoc PhoneNumber)

names :: PhoneBook -> Assoc Name
names (MkPhoneBook _ n _) = n

phones :: PhoneBook -> Assoc PhoneNumber
phones (MkPhoneBook _ _ p) = p

owner  :: PhoneBook -> Entry
owner (MkPhoneBook o _ _) = o
  
-- 4. Implement byName and byPhone, emptyBook, addToBook, fromEntries

byName :: Name -> PhoneBook -> Maybe Entry
byName n p = findEntry n (names p)

byPhone :: PhoneNumber -> PhoneBook -> Maybe Entry
byPhone ph p = findEntry ph (phones p)

emptyBook :: Entry -> PhoneBook
emptyBook e = MkPhoneBook e empty empty

addToBook :: Entry -> PhoneBook -> PhoneBook
addToBook e (MkPhoneBook o n p) = MkPhoneBook o (n <+> (singleton (name e) e)) (p <+> (singleton (phone e) e))

fromEntries :: Entry -> [Entry] -> PhoneBook
fromEntries e es = foldr addToBook (emptyBook e) es

-- 5. Implement the callerID function.

data Telephone =
  MkTelephone PhoneNumber (PhoneNumber -> IO ())

number  :: Telephone -> PhoneNumber
number (MkTelephone pn _) = pn

receive :: Telephone -> PhoneNumber -> IO ()
receive (MkTelephone _ r) = r

callerID :: PhoneBook -> Telephone
callerID p = MkTelephone (phone $ owner p) (\ph -> do
    case (byPhone ph p) of
        Nothing -> putStrLn (ci ++ (show ph))
        Just e -> putStrLn (ci ++ (name e))
    print "Ring ring")
    where ci = "called ID: "

-- 6. Calling someone

call :: PhoneBook -> [Telephone] -> IO ()
call p ts = do
    putStrLn "Who would you like to call?"
    n <- getLine
    case (byName n p) of
        Nothing -> putStrLn "No such entry!"
        Just e -> if (length t == 0)
                        then (putStrLn "The number you dialed does not exist")
                        else (receive (head t) (phone $ owner p))
            where   t = filter ((==(phone e)).number) ts
    return ()

-- examples -- do NOT change

bill,bob,jeb,val :: Entry
bill = mkEntry "Bill"      [32,444,123]
bob  = mkEntry "Bob"       [32,444,124]
jeb  = mkEntry "Jebediah"  [32,444,125]
val  = mkEntry "Valentina" [32,444,126]

billbook,bobbook,jebbook,valbook :: PhoneBook
billbook = fromEntries bill [bob,jeb]
bobbook  = fromEntries bob  [bill,jeb]
jebbook  = fromEntries jeb  [bill,bob,val]
valbook  = fromEntries val  [bill,bob,jeb]

telephones :: [Telephone]
telephones = map callerID [billbook,bobbook,jebbook,valbook]

-- 7. Complete the Index instance for Lookup

data Lookup k = MkLookup (k -> Maybe Entry)

lookup :: Lookup k -> (k -> Maybe Entry)
lookup (MkLookup f) = f

instance Index Lookup where
    findEntry k (MkLookup f) = f k
    empty = MkLookup (\_ -> Nothing)
    singleton k e = MkLookup (\i -> if (i == k) then (Just e) else Nothing)
    (<+>) l1 l2 = MkLookup (\i -> case (findEntry i l1) of
        Just e1 -> Just e1
        Nothing -> case (findEntry i l2) of
            Just e2 -> Just e2
            Nothing -> Nothing)