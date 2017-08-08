module System.Console.Haskeline.Brick where

import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads

import qualified Brick as B
import qualified Brick.BChan as BC
import qualified Graphics.Vty as V

import Control.Concurrent
import Control.Exception (throw)

import Debug.Trace


data Name = BrickWidgetName

data Widget e n = Widget { name :: n
                         , stateMVar :: MVar State
                         , stateCache :: Maybe State
                         , fromBrickChan :: Chan FromBrick
                         , toAppChan :: BC.BChan e
                         , toAppEventType :: ToBrick -> e
                         , fromAppEventType :: e -> Maybe ToBrick
                         }

data State = State { stateLines :: [String]
                   , stateCurrent :: (String, String)
                   , statePrevLinesShowing :: Int
                   }

initialWidget :: BC.BChan e
              -> (ToBrick -> e)
              -> (e -> Maybe ToBrick)
              -> n
              -> IO (Widget e n)
initialWidget toAppChan' toAppEventType' fromAppEventType' n = do
    stmv <- newMVar $ State { stateLines = []
                            , stateCurrent = ("", "")
                            , statePrevLinesShowing = 0
                            }
    ch <- newChan
    return $ Widget { name = n
                    , stateMVar = stmv
                    , stateCache = Nothing
                    , fromBrickChan = ch
                    , toAppChan = toAppChan'
                    , toAppEventType = toAppEventType'
                    , fromAppEventType = fromAppEventType'
                    }

data FromBrick = Resize Int Int | Key V.Key [V.Modifier]
data ToBrick = LayoutRequest (MVar (Maybe Layout))

data BrickError = LayoutNotDefined
    deriving (Show)

instance Exception BrickError where

-- TODO: maybe return type should be () if mutable style is kept
handleEvent :: (Eq n) => Widget e n
            -> B.BrickEvent n e -> B.EventM n (Widget e n)
handleEvent w (B.AppEvent e) =
    case (fromAppEventType w) e of
      Just (LayoutRequest mv) -> do
          me <- B.lookupExtent (name w)
          case trace "asked for extent" $ me of
            Just (B.Extent _ _ (wid, he) _) -> do
                liftIO $ putMVar mv (Just $ Layout wid he)
                return w
            Nothing -> return w
      Nothing -> return w

handleEvent w _ = return w

brickRunTerm :: Widget e n -> IO RunTerm
brickRunTerm w = do
    ch <- newChan
    let to = TermOps { getLayout = getLayout' w
                     , withGetEvent = undefined
                     , saveUnusedKeys = saveKeys ch
                     , evalTerm = undefined
                     , externalPrint = writeChan ch . ExternalPrint
                     }
    return $ RunTerm { putStrOut = putStrOut' (stateMVar w)
                     , termOps = Left to
                     , wrapInterrupt = id
                     , closeTerm = return ()
                     }

putStrOut' :: MVar State -> String -> IO ()
putStrOut' stmv str = do
    modifyMVar_ stmv $ \s ->
        return $ s { stateLines = str : stateLines s }

getLayout' :: Widget e n -> IO Layout
getLayout' w = do
    mv <- newEmptyMVar
    let e = toAppEventType w $ LayoutRequest mv
    BC.writeBChan (toAppChan w) e
    ml <- takeMVar mv
    case ml of
      Just l -> return l
      Nothing -> throw LayoutNotDefined

newtype BrickTerm m a = MkBrickTerm { unBrickTerm :: ReaderT (MVar State) m a }
    deriving ( MonadException, MonadIO, Monad, Applicative, Functor
             , MonadReader (MVar State)
             )

instance MonadTrans BrickTerm where
    lift = MkBrickTerm . lift

instance (MonadReader Layout m, MonadException m) => Term (BrickTerm m) where
    reposition _ _ = return ()

    moveToNextLine _ = MkBrickTerm $ do
        stmv <- ask
        liftIO $ modifyMVar_ stmv $ \s ->
            let (pre,suff) = stateCurrent s in
            return $ s { stateLines = (pre ++ suff) : stateLines s
                       , stateCurrent = ("", "")
                       }

    printLines ls = MkBrickTerm $ do
        stmv <- ask
        liftIO $ modifyMVar_ stmv $ \s ->
            return $ s { stateLines = (reverse ls) ++ stateLines s }

    drawLineDiff _ (pre, suff) = MkBrickTerm $ do
        stmv <- ask
        liftIO $ modifyMVar_ stmv $ \s -> return $
            s { stateCurrent = ( graphemesToString pre
                               , graphemesToString suff) }

    clearLayout = MkBrickTerm $ do
        stmv <- ask
        liftIO $ modifyMVar_ stmv $ \s ->
            return $ s { statePrevLinesShowing = 0 }

    ringBell _ = return ()


render :: Widget e n -> B.Widget n
render Widget { name = n, stateCache = Nothing } =
    B.reportExtent n $ B.str "nothing here yet!"
render Widget { name = n
              , stateCache = Just (
                    State { stateCurrent = (pre, suff) })
              } =
    B.reportExtent n $
        B.str pre B.<+> B.str suff
