import SnakeBoard
import Snake hiding (changeDir, turn)
import Graphics.Gloss.Interface.IO.Game
import System.Info(os)

worldScale = 14

configPath :: FilePath
configPath | os == "linux"   = "/home/sandor/Documents/Stuff/GlossSnake/Config"
           | os == "mingw32" = "C:\\Users\\Sandor\\Documents\\GlossSnake\\Config.txt"
           | otherwise       = "Config.txt"

colors = [white, red, orange, yellow, green, cyan, blue, rose]

currentColor :: World -> Color
currentColor w = colors !! (colorInd w)

displaySize = 350 -- Squareformed, half of a length

renderSnake :: World -> Picture
renderSnake = pictures.(map coordToRectangle).coords.snake

renderFoodPoint :: World -> Picture
renderFoodPoint = coordToRectangle.foodPos

renderBorders :: World -> Picture
renderBorders world = line [(-displaySize, -displaySize), (-displaySize, displaySize), (displaySize, displaySize), (displaySize,-displaySize), (-displaySize, -displaySize)]

renderWorld :: World -> IO Picture
renderWorld w = return $ pictures $ [color (currentColor w) $ rectangleSolid 2000 2000, renderSnake w, renderFoodPoint w, renderBorders w, renderScore w, renderVersion w] ++ ps ++ rm where
  ps = if paused w then pauseList else []
  rm = if restartMode w then menuList else []
  pauseList = [translate (-200)    50  $ text "PAUSE",
               translate (-200)  (-50) $ scale 0.2 0.2 $ text "Press c to change background color",
               translate (-200) (-100) $ scale 0.2 0.2 $ text "Press q to exit to menu"]

  menuList  = [translate (-200)   200  $ scale 0.2 0.2 $ text "Press 1 to change version to 1.0",
               translate (-200)   150  $ scale 0.2 0.2 $ text "Press 2 to change version to 2.0",
               translate (-200)   100  $ scale 0.2 0.2 $ text "Press c to change background color",
               translate (-200)    50  $ scale 0.2 0.2 $ text "Press q to quit game",
               translate (-200)  (-50) $ scale 0.2 0.2 $ text "Press space to start new game"]


renderScore :: World -> Picture
renderScore w = pictures [translate 350 300 $ scale 0.4 0.4 $ text $ "Score: " ++ (show $ len $ snake w), translate 350 250 $ scale 0.2 0.4 $ text $ "High Score: " ++ (show $ getHighScore w)]

renderVersion :: World -> Picture
renderVersion = (translate (-600) 300).(scale 0.3 0.3).text.("Version: " ++).(++".0").show.(+1).fromEnum.not.original

coordToRectangle :: Coord -> Picture
coordToRectangle (x, y) = translate ((fromIntegral x  + 0.5) * worldScale -350) ((fromIntegral y + 0.5) * worldScale -350) $ rectangleSolid (worldScale - 2) (worldScale - 2)

step :: Event -> World -> IO World
step (EventKey (Char 'q') Down _ _) w | paused w = saveWorld configPath w >> newWorldIO configPath (height w) (width w)
                                      | not $ restartMode w = return w
                                      | otherwise = saveWorld configPath w >> error "Quitting Game in worst way possible" 
step (EventKey k Down _ _) w = return $ f w where
  ps = paused w
  rm = restartMode w
  f = if rm then fRestart else if ps then fPause else case k of
    (Char 'w')                 -> changeDir N
    (Char 'a')                 -> changeDir W
    (Char 's')                 -> changeDir S
    (Char 'd')                 -> changeDir E
    (SpecialKey KeyUp)         -> changeDir N
    (SpecialKey KeyRight)      -> changeDir E
    (SpecialKey KeyDown)       -> changeDir S
    (SpecialKey KeyLeft)       -> changeDir W
    (MouseButton LeftButton)   -> turn False
    (MouseButton RightButton)  -> turn True
    (SpecialKey KeySpace)      -> switchPause
    _                          -> id
  fRestart = case k of
    (Char '1')                 -> setOriginal True
    (Char '2')                 -> setOriginal False
    (Char 'c')                 -> changeColor $ length colors
    (SpecialKey KeySpace)      -> switchRestart
    _                          -> id
  fPause = case k of
    (SpecialKey KeySpace)      -> switchPause
    (Char 'c')                 -> changeColor $ length colors
    _                          -> id
step _ s = return s

main :: IO ()
main = newWorldIO configPath 50 50 >>= \beginning -> playIO (InWindow "Snake" (1000, 800) (50, 50)) red 100 beginning renderWorld step advanceIO 


