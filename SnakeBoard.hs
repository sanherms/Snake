module SnakeBoard(World,
                  advanceIO,
                  newWorld,
                  changeDir,
                  snake,
                  foodPos,
                  height,
                  width,
                  getHighScore,
                  paused,
                  switchPause, 
                  switchRestart,
                  restartMode,
                  setOriginal,
                  original,
                  changeColor,
                  colorInd,
                  newWorldIO,
                  saveWorld,
                  turn) where

import qualified Snake as S
import System.Random(randomRIO)
import System.Directory(doesFileExist)

data World = World {snake       :: S.Snake, -- The Snake
                    foodPos     :: S.Coord, -- The position of the snake's food to "eat"
                    highScore   :: (Int, Int), --The highScores for both versions
                    height      :: Int, -- Height of the coordinate system
                    width       :: Int, -- Width of the coordinate sytem
                    paused      :: Bool, -- Is the game currently paused?
                    restartMode :: Bool, -- Is the game in the main menu?
                    original    :: Bool, -- Are we playing the original version (1.0)?
                    colorInd    :: Int, --  Index of the background color. Can be used for an own list of colors
                    speed       :: Float, -- 
                    speedCount  :: Float -- 
                   }

--Advances the world. Returns Nothing, when moveSafe fails. The first bool describes if a food point has been eaten in the step
advance :: World -> Maybe (Bool, World)
advance w = fmap (\s -> (S.isOn (foodPos w) $ snake w, w{snake=s, speedCount=0})) $ moveSafe (original w) (height w) (width w) (snake w)

--IO expansion of advance. Creates a new world, when advance returns Nothing
advanceIO :: Float -> World -> IO World
advanceIO f w | paused w || restartMode w = return w
              | not $ readyToMove w = return w{speedCount=speedCount w + f}
              | otherwise = case advance w of
  Nothing -> do
    nS <- randCoord (height w) (width w)
    let newSnake = S.new nS
    nFP <- newFoodPoint (height w) (width w) newSnake
    return $ setHighScore (max (S.len $ snake w) $ getHighScore w) $ w{snake=newSnake, foodPos=nFP, restartMode=True}
  (Just (ate, nW)) -> if not ate then return nW else do
    nFP <- newFoodPoint (height nW) (width nW) (snake nW)
    return w{snake=S.inc $ snake nW,foodPos=nFP} 

--Generates a random Coordinate
randCoord :: Int -> Int -> IO S.Coord
randCoord h w = do
    x <- randomRIO (0, w-1)
    y <- randomRIO (0, h-1)
    return (x, y)

--Generates a new food position
newFoodPoint :: Int -> Int -> S.Snake -> IO S.Coord
newFoodPoint h w s = do
    nFP <- randCoord h w
    if S.isOn nFP s then newFoodPoint h w s else return nFP

--Moves the snake. Returns Nothing if snake collides with itself or goes into the wall in version 1.0
moveSafe :: Bool -> Int -> Int -> S.Snake -> Maybe S.Snake
moveSafe or h w s = if S.isOn newPos s || (or && outOfBounds) then Nothing else Just $ S.add newPos tempS where
  newPos = (mod x w, mod y h) 
  tempS = S.continueDir s
  (x, y) = S.posFrom (S.curPos tempS) (S.dir tempS)
  outOfBounds = x < 0 || y < 0 || x >= h || y >= w

--Creates a new World with starting position of the snake, first food position, and width and height. Currently not in use
newWorld :: S.Coord -> S.Coord -> Int -> Int -> World
newWorld sp fp h w = World (S.new sp) fp (0, 0) h w False True True 0 0.1 0

--Creates a new world, with random positions. Reads saved data from config file.
newWorldIO :: FilePath -> Int -> Int -> IO World
newWorldIO fp h w = do
  sp <- randCoord h w
  let nS = S.new sp
  nFP <- newFoodPoint h w nS
  mHS <- readFileMaybe fp
  case mHS of
    Nothing -> return $ newWorld sp nFP h w
    (Just l) -> return.(\[hs1, hs2, cInd, or] -> World nS nFP (hs1, hs2) h w False True (toEnum or) cInd 0.05 0).(map read :: [String] -> [Int]).(take 4).lines $ l

--Saves data into config file
saveWorld :: FilePath -> World -> IO ()
saveWorld fp w = writeFile fp score where
  score = show hs1 ++ "\n" ++ show hs2 ++ "\n" ++ cInd ++ "\n" ++ or
  (hs1, hs2) = highScore w
  cInd = show $ colorInd w
  or = show $ fromEnum $ original w

--reads a file and return Nothing if it doesnÄt exist
readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe fp = do
  exists <- doesFileExist fp
  if exists then readFile fp >>= return.return else return Nothing

--Changes direction of the snake
changeDir :: S.Dir -> World -> World
changeDir d w = w{snake = S.changeDir d $ snake w}

--Changes direction of the snake by turning
turn :: Bool -> World -> World
turn cW w = w{snake = S.turn cW $ snake w}

--Switches the Pause Mode flag
switchPause :: World -> World
switchPause w = w{paused = not $ paused w}

--Switches the Restart Mode flag
switchRestart :: World -> World
switchRestart w = w{restartMode = not $ restartMode w}

-- Sets whether the version is 1.0 (True) or 2.0 (False)
setOriginal :: Bool -> World -> World
setOriginal o w = w{original=o}

--Returns high score for current game version
getHighScore :: World -> Int
getHighScore w = f $ highScore $ w where
  f = if original w then fst else snd 

--Sets a new high score for current game version
setHighScore :: Int -> World -> World
setHighScore nHS w = w{highScore=newHS} where
  (f, s) = highScore w
  newHS = if original w then (nHS, s) else (f, nHS)

--Changes the index of the color
changeColor :: Int -> World -> World
changeColor i w = w{colorInd=mod (colorInd w + 1) i}

readyToMove :: World -> Bool
readyToMove w = speed w <= speedCount w
