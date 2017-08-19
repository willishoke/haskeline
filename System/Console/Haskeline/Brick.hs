module System.Console.Haskeline.Brick ( configure
                                      , initialWidget
                                      , Widget
                                      , Config
                                      , ToBrick
                                      , handleEvent
                                      , useBrick
                                      , render
                                      ) where

import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads
import qualified System.Console.Haskeline.InputT as I
import qualified System.Console.Haskeline.Key as K

import qualified Control.Monad.Trans.Reader as Reader

import qualified Brick as B
import qualified Brick.BChan as BC
import qualified Graphics.Vty as V

import Control.Concurrent

data Config e = MkConfig { fromBrickChan :: Chan Event
                         , toAppChan :: BC.BChan e
                         , toAppEventType :: ToBrick -> e
                         , fromAppEventType :: e -> Maybe ToBrick
                         }

data Widget n = MkWidget { name :: n
                         , visibleLines :: [String]
                         , hiddenLines :: [String]
                         , current :: (String, String)
                         , extent :: Maybe (Int, Int)
                         }

configure :: BC.BChan e
          -> (ToBrick -> e)
          -> (e -> Maybe ToBrick)
          -> IO (Config e)
configure toAppChan' toAppEventType' fromAppEventType' = do
    ch <- newChan
    return $ MkConfig { fromBrickChan = ch
                      , toAppChan = toAppChan'
                      , toAppEventType = toAppEventType'
                      , fromAppEventType = fromAppEventType'
                      }

initialWidget :: n -> Widget n
initialWidget n = MkWidget { name = n
                           , visibleLines = []
                           , hiddenLines = []
                           , current = ("", "")
                           , extent = Nothing
                           }

data ToBrick = LayoutRequest (MVar (Maybe Layout))
             | MoveToNextLine
             | PrintLines [String]
             | DrawLineDiff LineChars
             | ClearLayout

handleEvent :: (Eq n) => Config e
            -> Widget n -> B.BrickEvent n e -> B.EventM n (Widget n)
handleEvent c w (B.AppEvent e) =
    case (fromAppEventType c) e of
      Just (LayoutRequest mv) -> do
          me <- B.lookupExtent (name w)
          case me of
            Just (B.Extent _ _ (wid, he) _) -> do
                liftIO . putMVar mv $ Just $ Layout wid he
                return $ w { extent = Just (wid, he) }
            Nothing -> do
                liftIO . putMVar mv $ Nothing
                return w

      Just MoveToNextLine -> do
          let (pre,suff) = current w
              w' = w { visibleLines = visibleLines w ++ [pre ++ suff]
                     , current = ("", "")
                     }
          let vp = B.viewportScroll (name w)
          B.vScrollToEnd vp
          return w'

      Just (PrintLines ls) -> do
          return $ w { visibleLines = visibleLines w ++ ls }

      Just (DrawLineDiff (pre, suff)) -> do
          return $ w { current = ( graphemesToString pre
                                 , graphemesToString suff) }

      Just ClearLayout -> do
          return $ w { visibleLines = []
                     , hiddenLines = hiddenLines w ++ visibleLines w
                     }

      Nothing -> return w

handleEvent c w (B.VtyEvent (V.EvKey k ms)) = do
    liftIO $ writeChan (fromBrickChan c) $ mkKeyEvent k
    return w
        where
            mkKeyEvent :: V.Key -> Event
            mkKeyEvent (V.KChar c') =
                KeyInput [ addModifiers ms $ K.simpleKey (K.KeyChar c') ]
            mkKeyEvent V.KEnter =
                KeyInput [ addModifiers ms $ K.simpleKey (K.KeyChar '\n') ]
            mkKeyEvent V.KBS =
                KeyInput [ addModifiers ms $ K.simpleKey K.Backspace ]
            mkKeyEvent V.KDel =
                KeyInput [ addModifiers ms $ K.simpleKey K.Delete ]
            mkKeyEvent V.KLeft =
                KeyInput [ addModifiers ms $ K.simpleKey K.LeftKey ]
            mkKeyEvent V.KRight =
                KeyInput [ addModifiers ms $ K.simpleKey K.RightKey ]
            mkKeyEvent V.KUp =
                KeyInput [ addModifiers ms $ K.simpleKey K.UpKey ]
            mkKeyEvent V.KDown =
                KeyInput [ addModifiers ms $ K.simpleKey K.DownKey ]
            mkKeyEvent _ = KeyInput []

            addModifiers :: [V.Modifier] -> K.Key -> K.Key
            addModifiers [] k' = k'
            addModifiers (V.MShift:tl) (K.Key m bc) =
                addModifiers tl $ (K.Key m { K.hasShift = True } bc)
            addModifiers (V.MCtrl:tl) (K.Key m (K.KeyChar c')) =
                addModifiers tl $ K.Key m (K.KeyChar $ K.setControlBits c')
            addModifiers (V.MCtrl:tl) k' = addModifiers tl . K.ctrlKey $ k'
            addModifiers (V.MMeta:tl) k' = addModifiers tl . K.metaKey $ k'
            addModifiers (V.MAlt:tl) k' = addModifiers tl k'

handleEvent _ w (B.VtyEvent (V.EvResize _ _)) = do
    me <- B.lookupExtent (name w)
    case me of
      Just (B.Extent _ _ (wid, he) _) -> do
          return $ w { extent = Just (wid, he) }
      Nothing -> return w

handleEvent _ w _ = return w

useBrick :: Config e -> I.Behavior
useBrick c = I.Behavior (brickRunTerm c)

brickRunTerm :: Config e -> IO RunTerm
brickRunTerm c = do
    let tops = TermOps { getLayout = getLayout'
                       , withGetEvent = withGetEvent'
                       , saveUnusedKeys = saveKeys (fromBrickChan c)
                       , evalTerm = evalBrickTerm c
                       , externalPrint =
                           writeChan (fromBrickChan c) . ExternalPrint
                       }
    return $ RunTerm { putStrOut = putStrOut'
                     , termOps = Left tops
                     , wrapInterrupt = id
                     , closeTerm = return ()
                     }
        where
            putStrOut' :: String -> IO ()
            putStrOut' str = do
                BC.writeBChan (toAppChan c) $
                    toAppEventType c $ PrintLines [str]

            getLayout' :: IO Layout
            getLayout' = do
                mv <- newEmptyMVar
                let e = toAppEventType c $ LayoutRequest mv
                BC.writeBChan (toAppChan c) e
                ml <- takeMVar mv
                case ml of
                  Just l -> return $ l
                  Nothing -> return $ Layout 0 0

            withGetEvent' :: forall m a . CommandMonad m
                          => (m Event -> m a) -> m a
            withGetEvent' f = f $ liftIO $ readChan (fromBrickChan c)

newtype BrickTerm m a =
    MkBrickTerm { unBrickTerm :: ReaderT (ToBrick -> IO ()) m a }
    deriving ( MonadException, MonadIO, Monad, Applicative, Functor
             , MonadReader (ToBrick -> IO ())
             )

instance MonadTrans BrickTerm where
    lift = MkBrickTerm . lift

evalBrickTerm :: (CommandMonad m) => Config e -> EvalTerm m
evalBrickTerm c = EvalTerm
    (runReaderT' send . unBrickTerm)
    (MkBrickTerm . lift)
        where send = BC.writeBChan (toAppChan c) . toAppEventType c

instance (MonadReader Layout m, MonadException m)
  => Term (BrickTerm m) where
    reposition _ _ = return ()

    moveToNextLine _ = do
        f <- ask
        liftIO $ f MoveToNextLine

    printLines ls = do
        f <- ask
        liftIO $ f $ PrintLines ls

    drawLineDiff _ d = do
        f <- ask
        liftIO $ f $ DrawLineDiff d

    clearLayout = do
        f <- ask
        liftIO $ f $ ClearLayout

    ringBell _ = return ()


render :: (Ord n, Show n) => Widget n -> B.Widget n
render (MkWidget { name = n
                 , current = (pre, suff)
                 , visibleLines = ls
                 , extent = mext
                 }) =
    B.reportExtent n $ B.viewport n B.Vertical  $ prev B.<=> curr
        where
            prev = B.vBox $ map B.str $ concat $ map (wrap mext) ls
            curr = B.visible $ B.showCursor n (loc mext) $
                B.vBox $ map B.str $ wrap mext $ pre ++ suff

            loc Nothing = B.Location (length pre, 0)
            loc (Just (w, _)) = let (q, r) = divMod (length pre) w in
                                    B.Location (r, q)

wrap :: Maybe (Int, Int) -> String -> [String]
wrap Nothing l = [l]
wrap (Just (w, _)) l = go l
    where go xs = case splitAt w xs of
                    (ys, []) -> [ys]
                    (ys, tl) -> ys : go tl
