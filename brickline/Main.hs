module Main where

import System.Console.Haskeline

import Brick
import Brick.BChan
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import Control.Monad (void)

data Event = InputLine String | EOF | Quit
data MyState = MyState { }
type Name = ()

initialState :: IO MyState
initialState = return $ MyState { }

app :: App MyState Event Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: MyState -> BrickEvent Name Event -> EventM Name (Next MyState)
handleEvent s (AppEvent (InputLine _)) = continue $ s
handleEvent s (AppEvent EOF) = halt s
handleEvent s (AppEvent Quit) = halt s
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent s _ = continue s

drawUI :: MyState -> [Widget Name]
drawUI _ = [C.center $ str "yo"]

theMap :: AttrMap
theMap = attrMap V.defAttr []

runHaskeline :: IO ()
runHaskeline = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
             Nothing -> return ()
             Just "quit" -> return ()
             Just input -> do
                 outputStr input
                 loop

main :: IO ()
main = do
    --runHaskeline
    --_ <- forkIO $ runHaskeline chan s
    chan <- newBChan 10
    s <- initialState
    void $ customMain (V.mkVty V.defaultConfig) (Just chan) app s
