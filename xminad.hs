{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

import XMonad
import XMonad.Actions.UpdateFocus
import XMonad.Config.Desktop
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run (spawnPipe)

-- local modules **************************************************************
import qualified XMonad.Local.Config as Local
import qualified XMonad.Local.EventHook as Local
import qualified XMonad.Local.LogHook as Local
import qualified XMonad.Local.Keys as Local
import qualified XMonad.Local.Layout as Local
import qualified XMonad.Local.ManageHook as Local
import qualified XMonad.Local.Mouse as Local
import qualified XMonad.Local.TopicSpace as Local
import qualified XMonad.Local.XConfig as Local

-- main module ****************************************************************

myConfig h = Local.xConfig
    { modMask = Local.modMask
    , borderWidth = 1
    , normalBorderColor = "#FFD12B"
    , focusedBorderColor = "#FF511F"
    , terminal = Local.terminal
    , workspaces = Local.workspaces
    , layoutHook = desktopLayoutModifiers Local.layoutHook
    , keys = Local.keyBindings
    , logHook = Local.logHook h
    , handleEventHook = Local.eventHook
    , manageHook = Local.manageHook
    , startupHook = myStartupHook
    , mouseBindings = Local.mouseBindings
    }
  where
    myStartupHook = do
        startupHook Local.xConfig
        adjustEventInput
        setWMName "LG3D"

main :: IO ()
main = do
    h <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ myConfig h
