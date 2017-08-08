module System.Console.Haskeline.Brick where

import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads
import qualified System.Console.Haskeline.Key as K

import qualified Control.Monad.Trans.Reader as Reader

import qualified Brick as B
import qualified Brick.BChan as BC
import qualified Graphics.Vty as V

import Control.Concurrent
import Control.Exception (throw)

--import Debug.Trace


data Name = BrickWidgetName

data Widget e n = Widget { name :: n
                         , stateMVar :: MVar State
                         , stateCache :: Maybe State
                         , fromBrickChan :: Chan Event
                         , toAppChan :: BC.BChan e
                         , toAppEventType :: ToBrick -> e
                         , fromAppEventType :: e -> Maybe ToBrick
                         }

data State = State { stateLines :: [String]
                   , stateCurrent :: (String, String)
                   , statePrevLinesShowing :: Int
                   }
                   deriving (Show)

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
    deriving (Show)
data ToBrick = LayoutRequest (MVar (Maybe Layout))
             | StateUpdated State

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
          liftIO . putMVar mv $ case me of
            Just (B.Extent _ _ (wid, he) _) -> Just $ Layout wid he
            Nothing -> Nothing
          return w
      Just (StateUpdated s) -> do
          let vp = B.viewportScroll (name w)
          B.vScrollToEnd vp
          return $ w { stateCache = Just s }
      Nothing -> return w

handleEvent w (B.VtyEvent (V.EvKey k ms)) = do
    liftIO $ writeChan (fromBrickChan w) $ transformEvent $ Key k ms
    return w

handleEvent w _ = return w

brickRunTerm :: Widget e n -> IO RunTerm
brickRunTerm w = do
    let to = TermOps { getLayout = getLayout' w
                     , withGetEvent = withGetEvent' (fromBrickChan w)
                     , saveUnusedKeys = saveKeys (fromBrickChan w)
                     , evalTerm = evalBrickTerm w
                     , externalPrint = writeChan (fromBrickChan w) . ExternalPrint
                     }
    return $ RunTerm { putStrOut = putStrOut' w
                     , termOps = Left to
                     , wrapInterrupt = id
                     , closeTerm = return ()
                     }

putStrOut' :: Widget e n -> String -> IO ()
putStrOut' w str = do
    --traceM $ "putStrOut': " ++ str
    s' <- modifyMVar (stateMVar w) $ \s -> do
        let s' = s { stateLines = str : stateLines s }
        return (s', s')
    BC.writeBChan (toAppChan w) $ toAppEventType w $ StateUpdated s'



getLayout' :: Widget e n -> IO Layout
getLayout' w = do
    --traceM "getLayout'"
    mv <- newEmptyMVar
    let e = toAppEventType w $ LayoutRequest mv
    BC.writeBChan (toAppChan w) e
    ml <- takeMVar mv
    case ml of
      Just l -> return $ l
      Nothing -> return $ Layout 10 10

transformEvent :: FromBrick -> Event
transformEvent (Resize _ _) = WindowResize
transformEvent (Key (V.KChar c) ms) =
    KeyInput [ addModifiers ms $ K.simpleKey (K.KeyChar c) ]
transformEvent (Key V.KEnter ms) =
    KeyInput [ addModifiers ms $ K.simpleKey (K.KeyChar '\n') ]
transformEvent (Key V.KBS ms) =
    KeyInput [ addModifiers ms $ K.simpleKey K.Backspace ]
transformEvent (Key V.KDel ms) =
    KeyInput [ addModifiers ms $ K.simpleKey K.Delete ]
transformEvent (Key V.KLeft ms) =
    KeyInput [ addModifiers ms $ K.simpleKey K.LeftKey ]
transformEvent (Key V.KRight ms) =
    KeyInput [ addModifiers ms $ K.simpleKey K.RightKey ]
transformEvent (Key V.KUp ms) =
    KeyInput [ addModifiers ms $ K.simpleKey K.UpKey ]
transformEvent (Key V.KDown ms) =
    KeyInput [ addModifiers ms $ K.simpleKey K.DownKey ]
transformEvent (Key _ _) = KeyInput []

addModifiers :: [V.Modifier] -> K.Key -> K.Key
addModifiers [] k = k
addModifiers (V.MShift:ms) (K.Key m bc) =
    addModifiers ms $ (K.Key m { K.hasShift = True } bc)
addModifiers (V.MCtrl:ms) (K.Key m (K.KeyChar c)) = addModifiers ms $
    K.Key m (K.KeyChar $ K.setControlBits c)
addModifiers (V.MCtrl:ms) k = addModifiers ms . K.ctrlKey $ k
addModifiers (V.MMeta:ms) k = addModifiers ms . K.metaKey $ k
addModifiers (V.MAlt:ms) k = addModifiers ms k

withGetEvent' :: forall m a . CommandMonad m
              => Chan Event -> (m Event -> m a) -> m a
withGetEvent' chanEvent f = f $ liftIO $ readChan chanEvent

newtype BrickTerm e n m a =
    MkBrickTerm { unBrickTerm :: ReaderT (Widget e n) m a }
    deriving ( MonadException, MonadIO, Monad, Applicative, Functor )

instance MonadTrans (BrickTerm e n) where
    lift = MkBrickTerm . lift

evalBrickTerm :: (CommandMonad m) => Widget e n -> EvalTerm m
evalBrickTerm w = EvalTerm
    (runReaderT' w . unBrickTerm)
    (MkBrickTerm . lift)

instance (MonadReader Layout m, MonadException m)
  => Term (BrickTerm e n m) where
    reposition _ _ = return ()

    moveToNextLine _ = MkBrickTerm $ do
        --traceM "moveToNextLine"
        w <- Reader.ask
        liftIO $ modifyMVar_ (stateMVar w) $ \s ->
            let (pre,suff) = stateCurrent s in
            return $ s { stateLines = (pre ++ suff) : stateLines s
                       , stateCurrent = ("", "")
                       }

    printLines ls = MkBrickTerm $ do
        --traceM "printLines"
        w <- Reader.ask
        liftIO $ modifyMVar_ (stateMVar w) $ \s ->
            return $ s { stateLines = (reverse ls) ++ stateLines s }

    drawLineDiff _ (pre, suff) = MkBrickTerm $ do
        --traceM $ "drawLineDiff: " ++ graphemesToString pre ++ graphemesToString suff
        w <- Reader.ask
        s' <- liftIO $ modifyMVar (stateMVar w) $ \s -> do
            let s' = s { stateCurrent = ( graphemesToString pre
                                        , graphemesToString suff) }
            return (s', s')
        liftIO $ BC.writeBChan (toAppChan w) $ toAppEventType w $ StateUpdated s'
        --traceM "drawLineDiff done"

    clearLayout = MkBrickTerm $ do
        --traceM "clearLayout"
        w <- Reader.ask
        liftIO $ modifyMVar_ (stateMVar w) $ \s ->
            return $ s { statePrevLinesShowing = 0 }

    ringBell _ = return ()


render :: (Ord n, Show n) => Widget e n -> B.Widget n
render (Widget { name = n, stateCache = Nothing }) =
    B.reportExtent n $ B.str "nothing here yet!"

render (Widget { name = n
               , stateCache = Just (
                    State { stateCurrent = (pre, suff)
                          , stateLines = lines})
               }) =

    B.viewport n B.Vertical $ B.reportExtent n $
        B.vBox (map (B.str) (reverse lines))
            B.<=>
                (B.str pre B.<+>
                    (B.showCursor n (B.Location (0,0))
                        (B.str suff)))
