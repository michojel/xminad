{-# LANGUAGE UnicodeSyntax #-}

module XMonad.Local.Operations where

import           System.Environment

import           XMonad
import qualified XMonad.Operations  as Op

restart ∷ X ()
restart = do
    progPath <- io getExecutablePath
    Op.restart progPath True
