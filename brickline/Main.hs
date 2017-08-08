{-# LANGUAGE LambdaCase #-}
module Main where

import System.Console.Haskeline
import qualified System.Console.Haskeline.Brick as HB

import Brick
import Brick.BChan
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V

import Control.Monad (void)
import Control.Concurrent (forkFinally)

import Control.Monad.IO.Class (liftIO)

data Event = InputLine String
           | EOF
           | Quit
           | FromHBWidget HB.ToBrick
           | HaskelineDied (Either SomeException ())

data Name = TheApp | HaskelineWidget
    deriving (Ord, Eq, Show)

data MyState = MyState { haskelineWidget :: HB.Widget Event Name }

initialState :: BChan Event -> IO MyState
initialState chan = do
    hw <- HB.initialWidget
            chan
            FromHBWidget
            (\case { FromHBWidget x -> Just x; _ -> Nothing })
            HaskelineWidget
    return $ MyState { haskelineWidget = hw }

app :: App MyState Event Name
app = App { appDraw = drawUI
          , appChooseCursor = \_ -> showCursorNamed HaskelineWidget
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: MyState -> BrickEvent Name Event -> EventM Name (Next MyState)
handleEvent s@MyState{haskelineWidget = hw} e = do
    hw' <- HB.handleEvent hw e
    handleAppEvent (s { haskelineWidget = hw' }) e

handleAppEvent :: MyState -> BrickEvent Name Event -> EventM Name (Next MyState)
handleAppEvent s (AppEvent (HaskelineDied e)) = do
    liftIO $ putStrLn $ show e
    continue s
handleAppEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleAppEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleAppEvent s _ = continue s

drawUI :: MyState -> [Widget Name]
drawUI s = [(C.center $ str "yo") <=>
    (B.border $ HB.render (haskelineWidget s))]

theMap :: AttrMap
theMap = attrMap V.defAttr []

runHaskeline :: HB.Widget Event Name -> IO ()
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
    chan <- newBChan 10
    s <- initialState chan
    _ <- forkFinally
            (runHaskeline $ haskelineWidget s)
            (writeBChan chan . HaskelineDied)
    void $ customMain (V.mkVty V.defaultConfig) (Just chan) app s
