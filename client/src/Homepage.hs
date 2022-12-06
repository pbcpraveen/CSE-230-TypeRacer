{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Homepage (getIP) where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

import Data.Word (Word8)
import Network.Socket
import Data.List.Split

data Name = Edit1
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor String Name
       }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)

        ui = C.center $
            (str "Enter IP: " <+> (hLimit 30 $ vLimit 5 e1)) <=>
            str " " <=>
            str "Enter to start."

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) =
    M.halt
appEvent ev = do
    r <- use focusRing
    case F.focusGetCurrent r of
      Just Edit1 -> zoom edit1 $ E.handleEditorEvent ev
      Nothing -> return ()

initialState :: St
initialState =
    St (F.focusRing [Edit1])
       (E.editor Edit1 Nothing "")

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

homepage :: M.App St e Name
homepage =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

getIP :: IO SockAddr
getIP = do
    st <- M.defaultMain homepage initialState
    let strIP = unlines $ E.getEditContents $ st^.edit1
    -- let strIP' = take (length strIP - 1) strIP
    let ipList = map (\s -> read s :: Word8) (splitOn "." strIP)
    let ipTuple = (ipList !! 0, ipList !! 1, ipList !! 2, ipList !! 3)
    return $ SockAddrInet 1234 (tupleToHostAddress ipTuple)
