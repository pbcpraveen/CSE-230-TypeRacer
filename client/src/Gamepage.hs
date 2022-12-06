module Gamepage where

import           Brick                  (App (..), AttrName, BrickEvent (..),
                                         EventM, Location (..),
                                         Padding (..), Widget, attrMap,
                                         attrName, defaultMain, continueWithoutRedraw,
                                         emptyWidget, fg, halt, padAll,
                                         padBottom, showCursor, showFirstCursor,
                                         str, withAttr, (<+>), (<=>))
import           Brick.Widgets.Center   (center)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isSpace)
import           Data.Maybe             (fromMaybe)
import           Data.Time              (getCurrentTime)
import           Data.Word              (Word8)
import           Graphics.Vty           (Attr, Color (..), Event (..), Key (..),
                                         Modifier (..), bold, defAttr,
                                         withStyle)
import           Text.Printf            (printf)

import           Data.Char  (isSpace)
import           Data.List  (groupBy, isPrefixOf)
import           Data.Maybe (fromJust, isJust)
import           Data.Time  (UTCTime, diffUTCTime)



data Position
  = BeforeCursor
  | AfterCursor

data State =
  State
    { target  :: String
    , input   :: String
    , start   :: Maybe UTCTime
    , end     :: Maybe UTCTime
    , strokes :: Integer
    , hits    :: Integer
    , loop    :: Bool
    }
data Character
  = Hit Char
  | Miss Char
  | Empty Char

type Line = [Character]

type Page = [Line]

initialState :: String -> State
initialState t =
  State
    { target = t
    , input = takeWhile isSpace t
    , start = Nothing
    , end = Nothing
    , strokes = 0
    , hits = 0
    , loop = False
    }

fgEmptyCode :: Word8
fgEmptyCode = 8

fgErrorCode :: Word8
fgErrorCode  = 1


startClock :: UTCTime -> State -> State
startClock now s = s {start = Just now}

stopClock :: UTCTime -> State -> State
stopClock now s = s {end = Just now}

hasStarted :: State -> Bool
hasStarted = isJust . start

hasEnded :: State -> Bool
hasEnded = isJust . end

cursorCol :: State -> Int
cursorCol = length . takeWhile (/= '\n') . reverse . input

cursorRow :: State -> Int
cursorRow = length . filter (== '\n') . input

cursor :: State -> (Int, Int)
cursor s = (cursorCol s, cursorRow s)

atEndOfLine :: State -> Bool
atEndOfLine s = cursorCol s == length (lines (target s) !! cursorRow s)

onLastLine :: State -> Bool
onLastLine s = cursorRow s + 1 == length (lines $ target s)

isComplete :: State -> Bool
isComplete s = input s == target s

isErrorFree :: State -> Bool
isErrorFree s = input s `isPrefixOf` target s

applyChar :: Char -> State -> State
applyChar c s =
  s'
    { hits =
        hits s' +
        if isErrorFree s'
          then 1
          else 0
    , strokes = strokes s + 1
    }
  where
    s'
      | isSpace c = s {input = input s ++ whitespace}
      | otherwise = s {input = input s ++ [c]}
    whitespace =
      case takeWhile isSpace . drop (length $ input s) $ target s of
        "" -> " "
        ws -> ws

applyBackspace :: State -> State
applyBackspace s = s {input = reverse . drop n . reverse $ input s}
  where
    n =
      case takeWhile (\(i, t) -> isSpace i && isSpace t) . reverse $
           zip (input s) (target s) of
        [] -> 1
        ws -> length ws

applyBackspaceWord :: State -> State
applyBackspaceWord s = s {input = reverse . drop n . reverse $ input s}
  where
    n = toWordBeginning . reverse $ input s
    toWordBeginning "" = 0
    toWordBeginning [c] = 1
    toWordBeginning (x:y:ys)
      | not (isSpace x) && isSpace y = 1
      | otherwise = 1 + toWordBeginning (y : ys)

character :: Position -> (Maybe Char, Maybe Char) -> Character
character _ (Just t, Just i)
  | t == i = Hit t
  | t /= i = Miss i
character _ (Nothing, Just i) = Miss i
character BeforeCursor (Just t, Nothing) = Miss t
character AfterCursor (Just t, Nothing) = Empty t

line :: Position -> (String, String) -> Line
line _ ("", "") = []
line p (ts, is) = map (character p) charPairs
  where
    charPairs = take maxLen $ zip (nothingsForever ts) (nothingsForever is)
    nothingsForever x = map Just x ++ repeat Nothing
    maxLen = max (length ts) (length is)

page :: State -> Page
page s = linesBeforeCursor ++ linesAfterCursor
  where
    linesBeforeCursor = map (line BeforeCursor) $ take (cursorRow s) linePairs
    linesAfterCursor = map (line AfterCursor) $ drop (cursorRow s) linePairs
    linePairs = zip (lines $ target s) (lines (input s) ++ repeat "")

noOfChars :: State -> Int
noOfChars = length . input

-- The following functions are only safe to use when both hasStarted and
-- hasEnded hold.
seconds :: State -> Double
seconds s = realToFrac $ diffUTCTime (fromJust $ end s) (fromJust $ start s)

countChars :: State -> Int
countChars = length . groupBy (\x y -> isSpace x && isSpace y) . target

wpm :: State -> Double
wpm s = fromIntegral (countChars s) / (5 * seconds s / 60)

accuracy :: State -> Double
accuracy s = fromIntegral (hits s) / fromIntegral (strokes s)
emptyAttrName :: AttrName
emptyAttrName = attrName "empty"

errorAttrName :: AttrName
errorAttrName = attrName "error"

resultAttrName :: AttrName
resultAttrName = attrName "result"

drawCharacter :: Character -> Widget ()
drawCharacter (Hit c)    = str [c]
drawCharacter (Miss ' ') = withAttr errorAttrName $ str ['_']
drawCharacter (Miss c)   = withAttr errorAttrName $ str [c]
drawCharacter (Empty c)  = withAttr emptyAttrName $ str [c]

drawLine :: Line -> Widget ()
-- We display an empty line as a single space so that it still occupies
-- vertical space.
drawLine [] = str " "
drawLine ls = foldl1 (<+>) $ map drawCharacter ls

drawText :: State -> Widget ()
drawText s = padBottom (Pad 2) . foldl (<=>) emptyWidget . map drawLine $ page s

drawResults :: State -> Widget ()
drawResults s =
  withAttr resultAttrName . str $
  printf "%.f words per minute • %.f%% accuracy" (wpm s) (accuracy s * 100)


draw :: State -> [Widget ()]
draw s
  | hasEnded s = pure . center . padAll 1 $ drawText s <=> drawResults s
  | otherwise =
    pure . center . padAll 1 . showCursor () (Location $ cursor s) $
    drawText s <=> str " "

-- handleChar :: Char -> State -> EventM () State ()
-- handleChar c s
--   | not $ hasStarted s = do
--     now <- liftIO getCurrentTime
--     let _ = startClock now s' 
--     continueWithoutRedraw 
--   | isComplete s' = do
--     now <- liftIO getCurrentTime
--     let _ = stopClock now s' 
--     continueWithoutRedraw
--   | otherwise = continueWithoutRedraw
--   where
--     s' = applyChar c s

appEvent :: BrickEvent () e -> EventM () State ()
appEvent (VtyEvent (EvKey KEsc [])) = halt

-- appEvent (VtyEvent (EvKey key [MCtrl])) =
--   case key of
--     KChar 'c' -> halt s
--     KChar 'd' -> halt s
--     KChar 'w' -> continue $ applyBackspaceWord s
--     KBS       -> continue $ applyBackspaceWord s
--     _         -> continue s

app :: Attr -> Attr -> Attr -> App State e ()
app emptyAttr errorAttr resultAttr =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = return ()
    , appAttrMap =
        const $
        attrMap
          defAttr
          [ (emptyAttrName, emptyAttr)
          , (errorAttrName, errorAttr)
          , (resultAttrName, resultAttr)
          ]
    }