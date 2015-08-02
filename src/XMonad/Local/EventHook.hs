module XMonad.Local.EventHook (eventHook) where

import Data.Monoid

import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks

eventHook :: Event -> X All
eventHook = mconcat
    [ ewmhDesktopsEventHook
    , docksEventHook
    , fadeWindowsEventHook
    , fullscreenEventHook
    ]
