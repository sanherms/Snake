module Snake(Snake,
             posFrom,
             new,
             add,
             inc,
             move,
             changeDir,
             isOn,
             Dir(..),
             Coord,
             len,
             coords,
             dir,
             curPos,
             turn) where

data Snake = Snake {len    :: Int, -- The maximum length of the snake
                    coords :: [(Int, Int)], -- The list of coordinates, the snake is currently on
                    dir    :: Dir -- The direction the snake is currently facing
                   }

data Dir = N | E | S | W deriving (Eq, Show, Enum, Bounded)

type Coord = (Int, Int)

-- Calculates the next coordinate from (x, y) in direction d
posFrom :: Coord -> Dir -> Coord
posFrom (x, y) d = case d of
  N -> (x,y+1)
  E -> (x+1,y)
  S -> (x,y-1)
  W -> (x-1,y)

--Creates a new snake if the length 1 on coordinate t
new :: (Int, Int) -> Snake
new t = Snake 1 [t] W

--Checks whether two directions are opposite of eachother
opFrom :: Dir -> Dir -> Bool
opFrom d1 d2 = elem (d1, d2) [(N, S), (E, W), (W, E), (S, N)]

--Returns the current position of the snake's head
curPos :: Snake -> Coord
curPos = last.coords

--Adds a coordinate to the snake. Removes oldest coordinate if maximum length is reached.
add :: Coord -> Snake -> Snake
add t (Snake l cs d) = Snake l (cList++[t]) d where
  cList = if l > length cs then cs else drop 1 cs

--Increases maximum length by 1
inc :: Snake -> Snake
inc (Snake l cs d) = Snake (l+1) cs d

--Moves the snake
move :: Snake -> Snake
move s = add (posFrom (curPos s) (dir s)) s

--Changes the direction of the snake. If the direction is opposite to the snakes current one, the snake is not changed.
changeDir :: Dir -> Snake -> Snake
changeDir d s = s{dir= if opFrom d $ dir s then dir s else d}

--Checks whether the snake is on coordinate c 
isOn :: Coord -> Snake -> Bool
isOn c = (elem c).coords

--Changes the direction by "turning" clockwise or counter-clockwise
turn :: Bool -> Snake -> Snake
turn clockW s = s{dir=newDir} where
  newDir = case (clockW, dir s) of
    (True, W)  -> N
    (False, N) -> W
    (cW, d) -> (if cW then succ else pred) d
