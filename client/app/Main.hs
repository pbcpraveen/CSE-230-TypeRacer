{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Brick
import qualified Brick.Main as M

import Control.Monad (void)

import TypeRacer (theMain)

main :: IO ()
main = theMain
