{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

import qualified DBus as D
import qualified DBus.Client as D

import XMonad
import XMonad.Actions.UpdateFocus
import XMonad.Config.Desktop
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

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

myConfig dbus = Local.xConfig
    { modMask = Local.modMask
    , borderWidth = 1
    , normalBorderColor = "#FFD12B"
    , focusedBorderColor = "#FF511F"
    , terminal = Local.terminal
    , workspaces = Local.workspaces
    , layoutHook = desktopLayoutModifiers Local.layoutHook
    , keys = Local.keyBindings
    , logHook = Local.logHook dbus
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

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
    D.requestName dbus (D.busName_ "org.xmonad.Log")
            [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
        >> return ()

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ withUrgencyHook NoUrgencyHook $ myConfig dbus
