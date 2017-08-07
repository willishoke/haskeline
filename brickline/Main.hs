module Main where

import System.Console.Haskeline
import qualified System.Console.Haskeline.Brick as HB

import Brick
import Brick.BChan
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import Control.Monad (void)

data Event = InputLine String | EOF | Quit
data Name = TheApp | HaskelineWidget
    deriving (Ord, Eq)
data MyState = MyState { haskelineWidget :: HB.Widget Name }

initialState :: IO MyState
initialState = do
    hw <- HB.initialWidget HaskelineWidget
    return $ MyState { haskelineWidget = hw }

app :: App MyState Event Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: MyState -> BrickEvent Name Event -> EventM Name (Next MyState)
handleEvent s@MyState{haskelineWidget = hw} e@(VtyEvent ve) = do
    hw' <- HB.handleEvent hw ve
    let s' = s { haskelineWidget = hw' }
    handleAppEvent s' e
handleEvent s e = handleAppEvent s e

handleAppEvent :: MyState -> BrickEvent Name Event -> EventM Name (Next MyState)
handleAppEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleAppEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleAppEvent s _ = continue s

drawUI :: MyState -> [Widget Name]
drawUI _ = [C.center $ str "yo"]

theMap :: AttrMap
theMap = attrMap V.defAttr []

runHaskeline :: HB.Widget Name -> IO ()
runHaskeline w = runInputTBehavior (useBrick w) defaultSettings loop
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
    --_ <- forkIO $ runHaskeline
    chan <- newBChan 10
    s <- initialState
    runHaskeline (haskelineWidget s)
    void $ customMain (V.mkVty V.defaultConfig) (Just chan) app s
