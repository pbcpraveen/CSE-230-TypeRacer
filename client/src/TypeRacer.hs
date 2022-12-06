{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module TypeRacer where

import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH
import Control.Monad (void, forever)

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.ProgressBar as P
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>), (<=>)
  , str
  , updateAttrMap
  , overrideAttr, withAttr
  )
import Brick.Util (fg, bg, on, clamp)
import qualified Brick.BChan
import qualified Graphics.Vty
import Control.Concurrent (threadDelay)
import GHC.Conc (forkIO)
import Data.Char (toLower)

data MyAppState n = MyAppState {
  _x, _y, _z :: Float,
  _corpus :: String,
  _typed :: String,
  _percentage :: Float
} deriving (Show)

makeLenses ''MyAppState

partition :: String -> String -> (String, String, String)
partition target actual = (common, wrong, rest)
  where
    commonLen = length $ takeWhile (uncurry (==)) $ zip target actual
    common = take commonLen actual
    wrong = drop commonLen actual
    rest = drop commonLen target

drawUI :: MyAppState () -> [Widget ()]
drawUI p = [ui]
    where
      -- use mapAttrNames
      -- xBar = updateAttrMap
      --        (A.mapAttrNames [ (xDoneAttr, P.progressCompleteAttr)
      --                        , (xToDoAttr, P.progressIncompleteAttr)
      --                        ]
      --        ) $ bar $ _x p
      -- -- or use individual mapAttrName calls
      -- yBar = updateAttrMap
      --        (A.mapAttrName yDoneAttr P.progressCompleteAttr .
      --         A.mapAttrName yToDoAttr P.progressIncompleteAttr) $
      --        bar $ _y p
      -- -- or use overrideAttr calls
      -- zBar = overrideAttr P.progressCompleteAttr zDoneAttr $
      --        overrideAttr P.progressIncompleteAttr zToDoAttr $
      --        bar $ _z p
      -- lbl c = Just $ show $ fromEnum $ c * 100
      -- bar v = P.progressBar (lbl v) v
      (common, wrong, rest) = partition (_corpus p) (_typed p)
      common' = withAttr correctAttr $ str common
      wrong' = withAttr wrongAttr $ str wrong
      rest' = withAttr restAttr $ str rest
      ui = --(str "X: " <+> xBar) <=>
           --(str "Y: " <+> yBar) <=>
           --(str "Z: " <+> zBar) <=>
           common' <+> wrong' <+> rest'


data TimerEvent = Interrupt deriving (Show)

valid :: Float -> Float
valid = clamp (0.0 :: Float) 1.0

computePercentage :: String -> String -> Float
computePercentage target actual = fromIntegral common / fromIntegral len
  where
    common = length $ takeWhile (uncurry (==)) $ zip target actual
    len = length target

appEventHandler :: T.BrickEvent () TimerEvent -> T.EventM () (MyAppState ()) ()
appEventHandler (T.AppEvent Interrupt) = z %= valid . (+ 0.01)
appEventHandler (T.VtyEvent e) = case e of
  V.EvKey V.KEsc        [] -> M.halt
  V.EvKey V.KBS         [] -> do
    before <- use typed
    let after = take (length before - 1) before
    typed .= after
    target <- use corpus
    actual <- use typed
    percentage .= computePercentage target actual
  V.EvKey (V.KChar  c ) [] -> do
    typed %= (++ [c])
    target <- use corpus
    actual <- use typed
    percentage .= computePercentage target actual
  _                        -> return ()
appEventHandler _ = return ()

xDoneAttr, xToDoAttr :: A.AttrName
xDoneAttr = A.attrName "X:done"
xToDoAttr = A.attrName "X:remaining"

yDoneAttr, yToDoAttr :: A.AttrName
yDoneAttr = A.attrName "Y:done"
yToDoAttr = A.attrName "Y:remaining"

zDoneAttr, zToDoAttr :: A.AttrName
zDoneAttr = A.attrName "Z:done"
zToDoAttr = A.attrName "Z:remaining"

correctAttr :: A.AttrName
correctAttr = A.attrName "correct"

wrongAttr :: A.AttrName
wrongAttr = A.attrName "wrong"

restAttr :: A.AttrName
restAttr = A.attrName "rest"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
         [ (xDoneAttr,                 V.black `on` V.white)
         , (xToDoAttr,                 V.white `on` V.black)
         , (yDoneAttr,                 V.magenta `on` V.yellow)
         , (zDoneAttr,                 V.blue `on` V.green)
         , (zToDoAttr,                 V.blue `on` V.red)
         , (correctAttr,               V.withStyle (fg $ V.RGBColor 0   150 0) V.bold)
         , (wrongAttr,                 V.withStyle (fg $ V.RGBColor 150 0   0) V.bold)
         , (restAttr,                  fg $ V.RGBColor 200 200 200)
         , (P.progressIncompleteAttr,  fg V.yellow)
         ]

theApp :: M.App (MyAppState ()) TimerEvent ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEventHandler
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

initialState :: MyAppState ()
initialState = MyAppState 0.25 0.18 0.63 "Hello, world!" "" 0.0

interruptThread :: Brick.BChan.BChan TimerEvent -> IO ()
interruptThread chan = do
  Brick.BChan.writeBChan chan Interrupt
  threadDelay 1000000

theMain :: IO ()
theMain = do
  eventChan <- Brick.BChan.newBChan 10

  void $ forkIO $ forever $ interruptThread eventChan

  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty
  finalState <- M.customMain initialVty buildVty (Just eventChan) theApp initialState
  print finalState
  return ()
