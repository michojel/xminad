{-# OPTIONS -fno-warn-missing-signatures #-}

module XMonad.Local.XConfig (xConfig) where

import XMonad
import XMonad.Config.Desktop

import qualified XMonad.Local.Keys as Local

xConfig = desktopConfig
    { XMonad.modMask = Local.modMask
    , XMonad.keys    = Local.keyBindings
    }
