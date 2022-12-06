{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module TypeRacer (theMain) where

import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH
import Control.Monad (void)

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
  , overrideAttr
  )
import Brick.Util (fg, bg, on, clamp)
import qualified Brick.BChan
import qualified Graphics.Vty

data MyAppState n = MyAppState { _x, _y, _z :: Float }

makeLenses ''MyAppState

drawUI :: MyAppState () -> [Widget ()]
drawUI p = [ui]
    where
      -- use mapAttrNames
      xBar = updateAttrMap
             (A.mapAttrNames [ (xDoneAttr, P.progressCompleteAttr)
                             , (xToDoAttr, P.progressIncompleteAttr)
                             ]
             ) $ bar $ _x p
      -- or use individual mapAttrName calls
      yBar = updateAttrMap
             (A.mapAttrName yDoneAttr P.progressCompleteAttr .
              A.mapAttrName yToDoAttr P.progressIncompleteAttr) $
             bar $ _y p
      -- or use overrideAttr calls
      zBar = overrideAttr P.progressCompleteAttr zDoneAttr $
             overrideAttr P.progressIncompleteAttr zToDoAttr $
             bar $ _z p
      lbl c = Just $ show $ fromEnum $ c * 100
      bar v = P.progressBar (lbl v) v
      ui = (str "X: " <+> xBar) <=>
           (str "Y: " <+> yBar) <=>
           (str "Z: " <+> zBar) <=>
           str "" <=>
           str "Hit 'x', 'y', or 'z' to advance progress, or 'q' to quit"

newtype TimerEvent = Interrupt ()
myEventHandler :: T.BrickEvent () TimerEvent -> T.EventM () (MyAppState ()) ()
myEventHandler _ = 
       let valid = clamp (0.0 :: Float) 1.0 in
       z %= valid . (+ 0.01)
  

appEventHandler :: T.BrickEvent () e -> T.EventM () (MyAppState ()) ()
appEventHandler (T.VtyEvent e) =
    let valid = clamp (0.0 :: Float) 1.0
    in case e of
         V.EvKey (V.KChar 'x') [] -> x %= valid . (+ 0.05)
         V.EvKey (V.KChar 'y') [] -> y %= valid . (+ 0.03)
         V.EvKey (V.KChar 'z') [] -> z %= valid . (+ 0.02)
         V.EvKey (V.KChar 'q') [] -> M.halt
         _ -> return ()
appEventHandler _ = return ()

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

xDoneAttr, xToDoAttr :: A.AttrName
xDoneAttr = theBaseAttr <> A.attrName "X:done"
xToDoAttr = theBaseAttr <> A.attrName "X:remaining"

yDoneAttr, yToDoAttr :: A.AttrName
yDoneAttr = theBaseAttr <> A.attrName "Y:done"
yToDoAttr = theBaseAttr <> A.attrName "Y:remaining"

zDoneAttr, zToDoAttr :: A.AttrName
zDoneAttr = theBaseAttr <> A.attrName "Z:done"
zToDoAttr = theBaseAttr <> A.attrName "Z:remaining"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
         [ (theBaseAttr,               bg V.brightBlack)
         , (xDoneAttr,                 V.black `on` V.white)
         , (xToDoAttr,                 V.white `on` V.black)
         , (yDoneAttr,                 V.magenta `on` V.yellow)
         , (zDoneAttr,                 V.blue `on` V.green)
         , (zToDoAttr,                 V.blue `on` V.red)
         , (P.progressIncompleteAttr,  fg V.yellow)
         ]

theApp :: M.App (MyAppState ()) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEventHandler
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

initialState :: MyAppState ()
initialState = MyAppState 0.25 0.18 0.63

theMain :: IO ()
theMain = do
  eventChan <- Brick.BChan.newBChan 10
  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty
  finalState <- M.customMain initialVty buildVty (Just eventChan) theApp initialState
  return ()
