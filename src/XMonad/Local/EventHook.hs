module XMonad.Local.EventHook (eventHook) where

import qualified Data.Map as M
import Data.Monoid

import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W

eventHook :: Event -> X All
eventHook = mconcat
    [ ewmhDesktopsEventHook
    , docksEventHook
    , fadeWindowsEventHook
    , fullscreenEventHook
    ]
