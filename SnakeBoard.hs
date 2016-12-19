module SnakeBoard(World, advanceIO, newWorld, changeDir, snake, foodPos, height, width, getHighScore, paused, switchPause, switchRestart, restartMode, setOriginal, original, changeColor, colorInd, newWorldIO, saveWorld, turn) where

import qualified Snake as S
import System.Random(randomRIO)
import System.Directory(doesFileExist)

data World = World {snake :: S.Snake, foodPos :: S.Coord, highScore :: (Int, Int), height :: Int, width :: Int, paused :: Bool, restartMode :: Bool, original :: Bool, colorInd :: Int}

advance :: World -> Maybe (Bool, World)
advance w = fmap (\s -> (S.isOn (foodPos w) $ snake w, w{snake=s})) $ moveSafe (original w) (height w) (width w) (snake w)

advanceIO :: World -> IO World
advanceIO w = if paused w || restartMode w then return w else case advance w of
  Nothing -> do
    nS <- randCoord (height w) (width w)
    let newSnake = S.new nS
    nFP <- newFoodPoint (height w) (width w) newSnake
    return $ setHighScore (max (S.len $ snake w) $ getHighScore w) $ w{snake=newSnake, foodPos=nFP, restartMode=True}
  (Just (ate, nW)) -> if not ate then return nW else do
    nFP <- newFoodPoint (height nW) (width nW) (snake nW)
    return w{snake=S.inc $ snake nW,foodPos=nFP} 

randCoord :: Int -> Int -> IO S.Coord
randCoord h w = do
    x <- randomRIO (0, w-1)
    y <- randomRIO (0, h-1)
    return (x, y)

newFoodPoint :: Int -> Int -> S.Snake -> IO S.Coord
newFoodPoint h w s = do
    nFP <- randCoord h w
    if S.isOn nFP s then newFoodPoint h w s else return nFP

moveSafe :: Bool -> Int -> Int -> S.Snake -> Maybe S.Snake
moveSafe or h w s = if S.isOn newPos s || (or && outOfBounds) then Nothing else Just $ S.add newPos s where
  newPos = (mod x w, mod y h) 
  (x, y) = S.posFrom (S.curPos s) (S.dir s)
  outOfBounds = x < 0 || y < 0 || x >= h || y >= w

newWorld :: S.Coord -> S.Coord -> Int -> Int -> World
newWorld sp fp h w = World (S.new sp) fp (0, 0) h w False True True 0

newWorldIO :: FilePath -> Int -> Int -> IO World
newWorldIO fp h w = do
  sp <- randCoord h w
  let nS = S.new sp
  nFP <- newFoodPoint h w nS
  mHS <- readFileMaybe fp
  case mHS of
    Nothing -> return $ newWorld sp nFP h w
    (Just l) -> return.(\[hs1, hs2, cInd, or] -> World nS nFP (hs1, hs2) h w False True (toEnum or) cInd).(map read :: [String] -> [Int]).(take 4).lines $ l

saveWorld :: FilePath -> World -> IO ()
saveWorld fp w = writeFile fp score where
  score = show hs1 ++ "\n" ++ show hs2 ++ "\n" ++ cInd ++ "\n" ++ or
  (hs1, hs2) = highScore w
  cInd = show $ colorInd w
  or = show $ fromEnum $ original w

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe fp = do
  exists <- doesFileExist fp
  if exists then readFile fp >>= return.return else return Nothing

changeDir :: S.Dir -> World -> World
changeDir d w = w{snake = S.changeDir d $ snake w}

turn :: Bool -> World -> World
turn cW w = w{snake = S.turn cW $ snake w}

switchPause :: World -> World
switchPause w = w{paused = not $ paused w}

switchRestart :: World -> World
switchRestart w = w{restartMode = not $ restartMode w}

setOriginal :: Bool -> World -> World
setOriginal o w = w{original=o}

getHighScore :: World -> Int
getHighScore w = f $ highScore $ w where
  f = if original w then fst else snd 

setHighScore :: Int -> World -> World
setHighScore nHS w = w{highScore=newHS} where
  (f, s) = highScore w
  newHS = if original w then (nHS, s) else (f, nHS)

changeColor :: Int -> World -> World
changeColor i w = w{colorInd=mod (colorInd w + 1) i}
