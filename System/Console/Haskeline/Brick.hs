module System.Console.Haskeline.Brick where

import System.Console.Haskeline.Term

import qualified Brick as B
import qualified Graphics.Vty as V

import Control.Concurrent

data Name = BrickWidgetName

data Widget n = Widget { stateMVar :: MVar State }

data State = State { stateLines :: [String]
                   , stateCurrent :: (String, String)
                   }

initialWidget :: n -> IO (Widget n)
initialWidget _ = do
    stmv <- newMVar $ State { stateLines = []
                            , stateCurrent = ("", "")
                            }
    return $ Widget { stateMVar = stmv }

data FromBrick = Halt | Key V.Key [V.Modifier]
data ToBrick = Draw

handleEvent :: Widget n -> V.Event -> B.EventM n (Widget n)
handleEvent w _ = return w

brickRunTerm :: Widget n -> IO RunTerm
brickRunTerm Widget { stateMVar = stmv } = do
    return $ RunTerm { putStrOut = myPutStrOut stmv
                     , termOps = undefined
                     , wrapInterrupt = id
                     , closeTerm = return ()
                     }

myPutStrOut :: MVar State -> String -> IO ()
myPutStrOut stmv str = do
    modifyMVar_ stmv (\s ->
        return $ s { stateLines = str : stateLines s })
