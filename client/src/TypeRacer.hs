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
import Brick.Types (Widget)
import Brick.Widgets.Core
  ( (<+>), (<=>)
  , str
  , updateAttrMap
  , withAttr, emptyWidget, vLimit
  )
import Brick.Util (fg, bg, on, clamp)
import qualified Brick.BChan
import qualified Graphics.Vty
import Control.Concurrent (threadDelay)
import GHC.Conc (forkIO)

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as BS  -- avoids putStrLn conflict
import Control.Monad.IO.Class
import System.Timeout (timeout)
import Data.List.Split

import Homepage (getIP)

recvTimeout :: Int
recvTimeout = 10000  -- 0.01 second

data MyAppState n = MyAppState {
  _corpus     :: String,
  _typed      :: String,
  _percentage :: Float,
  _sock       :: Socket,
  _racers     :: [(String, Int, Float)]
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
      (common, wrong, rest) = partition (_corpus p) (_typed p)
      common' = withAttr correctAttr $ str common
      wrong' = withAttr wrongAttr $ str wrong
      rest' = withAttr restAttr $ str rest

      barWidget [] = emptyWidget
      barWidget [x] = x
      barWidget _ = foldl1 (\b1 b2 -> b1 <=> str "\n" <=> b2) bars
      lbl v r | r > 100   = Just $ show (fromEnum $ if v < 0 then 0 else v*100) ++ "%"
              | otherwise = Just $ "Finished " ++ show r
      bar v r = P.progressBar (lbl v r) v

      bars = map (\(player, rank, prog) -> vLimit 10 (str (player ++ "  ") <+> updateAttrMap
             (A.mapAttrNames [ (xDoneAttr, P.progressCompleteAttr)
                             , (xToDoAttr, P.progressIncompleteAttr)
                             ]
             ) (bar prog rank))) (_racers p)
    
      ui = 
           barWidget bars <=>
           str "\n" <=>
           (vLimit 20 common' <+> wrong' <+> rest')


data TimerEvent = Interrupt deriving (Show)

valid :: Float -> Float
valid = clamp (0.0 :: Float) 1.0

computePercentage :: String -> String -> Float
computePercentage target actual = fromIntegral common / fromIntegral len
  where
    common = length $ takeWhile (uncurry (==)) $ zip target actual
    len = length target

sendProgress :: Socket -> Float -> IO ()
sendProgress sock prog = sendAll sock (BS.pack $ '|':show prog) 

handleReceive :: T.EventM () (MyAppState ()) ()
handleReceive = do
  sock <- use sock
  byteStrProgresses <- liftIO (timeout recvTimeout (recv sock 1024))
  case byteStrProgresses of
    Nothing -> return ()
    Just byteStrProgresses' -> do
      racers .= map step progresses
      where strProgresses = BS.unpack byteStrProgresses'
            progresses = map (splitOn ",") $ splitOn "|" strProgresses
            step [name, rank, prog] = (name, read rank :: Int, read prog :: Float)
            step _                  = error "Invalid progress"

appEventHandler :: T.BrickEvent () TimerEvent -> T.EventM () (MyAppState ()) ()
appEventHandler (T.AppEvent Interrupt) = handleReceive
appEventHandler (T.VtyEvent e) = case e of
  V.EvKey V.KEsc        [] -> M.halt
  V.EvKey V.KBS         [] -> do
    before <- use typed
    let after = take (length before - 1) before
    typed .= after
    target <- use corpus
    actual <- use typed
    percentage .= computePercentage target actual
    sock <- use sock
    prog <- use percentage
    liftIO $ sendProgress sock prog

  V.EvKey (V.KChar  c ) [] -> do
    typed %= (++ [c])
    target <- use corpus
    actual <- use typed
    percentage .= computePercentage target actual
    sock <- use sock
    prog <- use percentage
    liftIO $ sendProgress sock prog
  _                        -> return ()
appEventHandler _ = return ()

xDoneAttr, xToDoAttr :: A.AttrName
xDoneAttr = A.attrName "X:done"
xToDoAttr = A.attrName "X:remaining"

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
         , (correctAttr,               V.withStyle (fg $ V.RGBColor 0   150 0) V.bold)
         , (wrongAttr,                 V.withStyle (fg $ V.RGBColor 150 0   0) V.bold)
         , (restAttr,                  fg $ V.RGBColor 200 200 200)
         ]

theApp :: M.App (MyAppState ()) TimerEvent ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEventHandler
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

interruptThread :: Brick.BChan.BChan TimerEvent -> IO ()
interruptThread chan = do
  Brick.BChan.writeBChan chan Interrupt
  threadDelay 50000

theMain :: IO ()
theMain = do

  addr <- getIP

  sock <- socket AF_INET Stream 0
  connect sock addr
  welcome <- recv sock 1024
  print welcome
  textToType <- recv sock 1024

  let initialState = MyAppState (BS.unpack textToType) "" 0.0 sock []

  eventChan <- Brick.BChan.newBChan 10

  void $ forkIO $ forever $ interruptThread eventChan

  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty
  finalState <- M.customMain initialVty buildVty (Just eventChan) theApp initialState
  print finalState
  return ()
