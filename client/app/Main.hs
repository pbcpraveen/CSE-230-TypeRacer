{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

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

import           Brick                  (App (..), AttrName, BrickEvent (..),
                                         EventM, Location (..),
                                         Padding (..), Widget, attrMap,
                                         attrName, defaultMain,
                                         emptyWidget, fg, halt, padAll,
                                         padBottom, showCursor, showFirstCursor,
                                         str, withAttr, (<+>), (<=>))
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)
import Homepage  as H
import Gamepage as G

corpus :: String
corpus = "The quick brown fox jumps over the lazy dog"

main :: IO ()
main = do
    st <- M.defaultMain H.homepage H.initialState
    s <- M.defaultMain (G.app emptyAttr errorAttr resultAttr) (G.initialState corpus)
    return () 
    where 
           emptyAttr = fg . V.ISOColor $ G.fgEmptyCode
           errorAttr = flip V.withStyle V.bold . fg . V.ISOColor $ G.fgErrorCode
           resultAttr = fg . V.ISOColor $ G.fgErrorCode
