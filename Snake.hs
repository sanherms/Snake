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

data Snake = Snake {len    :: Int,
                    coords :: [(Int, Int)],
                    dir    :: Dir}

data Dir = N | E | S | W deriving (Eq, Show, Enum, Bounded)

type Coord = (Int, Int)

posFrom :: Coord -> Dir -> Coord
posFrom (x, y) d = case d of
  N -> (x,y+1)
  E -> (x+1,y)
  S -> (x,y-1)
  W -> (x-1,y)

new :: (Int, Int) -> Snake
new t = Snake 1 [t] W

opFrom :: Dir -> Dir -> Bool
opFrom d1 d2 = elem (d1, d2) [(N, S), (E, W), (W, E), (S, N)]

curPos :: Snake -> Coord
curPos = last.coords

add :: Coord -> Snake -> Snake
add t (Snake l cs d) = Snake l (cList++[t]) d where
  cList = if l > length cs then cs else drop 1 cs

inc :: Snake -> Snake
inc (Snake l cs d) = Snake (l+1) cs d

move :: Snake -> Snake
move s = add (posFrom (curPos s) (dir s)) s

changeDir :: Dir -> Snake -> Snake
changeDir d s = s{dir= if opFrom d $ dir s then dir s else d}

isOn :: Coord -> Snake -> Bool
isOn c = (elem c).coords

turn :: Bool -> Snake -> Snake
turn clockW s = s{dir=newDir} where
  newDir = case (clockW, dir s) of
    (True, W)  -> N
    (False, N) -> W
    (cW, d) -> (if cW then succ else pred) d
