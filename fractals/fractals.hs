module Fractals where

import Data.List
import Data.Maybe
import Data.Fixed

-- Turtles

data Turtle = Empty | Turn Double Turtle | Step Double Turtle
    deriving Show

done :: Turtle
done = Empty

turn :: Double -> Turtle
turn t = Turn t done

step :: Double -> Turtle
step d = Step d done

(>>>) :: Turtle -> Turtle -> Turtle
(>>>) (Empty) t = t
(>>>) (Turn d t') t = Turn d (t' >>> t)
(>>>) (Step d t') t = Step d (t' >>> t)

square :: Turtle
square = side >>> side >>> side >>> (step 50.0)
    where side = (step 50.0) >>> (turn 90.0)

--- Visualization

type Point = (Double,Double)
type Line = (Point,Point)

start :: Point
start = (700.0,100.0)

shift :: Point -> Double -> Double -> Point
shift (x,y) l d = (x + l * (sin(d*2*pi/360.0)), y + l * (cos(d*2*pi/360.0)))

turtleToLines :: Turtle -> [Line]
turtleToLines t = turtleToLines' t start 0.0 []
turtleToLines' t pos rot ls = case t of
    Empty -> ls
    (Turn r t') -> turtleToLines' t' pos (mod' (rot+r) 360.0) ls
    (Step d t') -> turtleToLines' t' newpos rot (ls ++ [(pos,newpos)])
        where   newpos = shift pos d rot

linesToSVG :: [Line] -> String
linesToSVG ls = heading ++ linesToSVG' ls
    where heading = "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
linesToSVG' [] = "</svg>"
linesToSVG' (((x1,y1),(x2,y2)):ls) = "<line x1=\"" ++ show x1 ++ "\" y1=\"" ++ show y1 ++ "\" x2=\"" ++ show x2 ++ "\" y2=\"" ++ show y2 ++ "\" stroke=\"blue\" stroke-width=\"4\"/>" ++ (linesToSVG' ls)

writeSVG :: FilePath -> Turtle -> IO ()
writeSVG p t = writeFile p (linesToSVG $ turtleToLines t)

-- Fractals

data Fractal = FEmpty | FTurn Double Fractal | FStep Fractal
    deriving Show

fdone :: Fractal
fdone = FEmpty

fturn :: Double -> Fractal
fturn t = FTurn t fdone

fstep :: Fractal
fstep = FStep fdone

(>->) :: Fractal -> Fractal -> Fractal
(>->) (FEmpty) f = f
(>->) (FTurn r f') f = FTurn r (f' >-> f)
(>->) (FStep f') f = FStep (f' >-> f)

concretize :: Double -> Fractal -> Turtle
concretize _ (FEmpty) = done
concretize d (FTurn r f) = Turn r (concretize d f)
concretize d (FStep f) = Step d (concretize d f)

refine :: Fractal -> Fractal -> Fractal
refine _ FEmpty = fdone
refine e (FTurn r f) = FTurn r (refine e f)
refine e (FStep f) = e >-> (refine e f)

times :: Int -> (a -> a) -> (a -> a)
times 0 _ x = x
times n f x = f (times (n-1) f x)

exam :: Fractal -> Fractal -> Int -> Double -> FilePath -> IO ()
exam f e n d fp = writeSVG fp (concretize d $ times n (refine e) f)

-- Test

expansion =
    fstep >-> fturn (60)   >->
    fstep >-> fturn (-120) >->
    fstep >-> fturn (60)   >->
    fstep

program =
    fstep >-> fturn (-120) >->
    fstep >-> fturn (-120) >->
    fstep


