{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

import qualified Data.Map as M
import Data.Monoid
import qualified DBus as D
import qualified DBus.Client as D

import XMonad
import qualified XMonad.Actions.FlexibleResize as FlexR
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import qualified XMonad.Util.EZConfig as EZ

-- local modules **************************************************************
import qualified XMonad.Local.Config as Local
import qualified XMonad.Local.LogHook as Local
import qualified XMonad.Local.Keys as Local
import qualified XMonad.Local.Layout as Local
import qualified XMonad.Local.ManageHook as Local
import qualified XMonad.Local.TopicSpace as Local

myBaseConfig = desktopConfig
    { XMonad.modMask = Local.modMask
    }

-- Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig Layout
                -> M.Map (ButtonMask, Button) (Window -> X())
myMouseBindings (XConfig {XMonad.modMask = mm}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((mm, button1), \w -> focus w >> mouseMoveWindow w
                                    >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((mm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((mm, button3), \w -> focus w >> FlexR.mouseResizeWindow w)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((mm, button4), const $ windows W.swapDown)
    , ((mm, button5), const $ windows W.swapUp)
    ]

-- | Enables 'focusFollowsMouse' for tiled windows only.  For this to
-- work you need to turn off 'focusFollowsMouse' in your configuration
-- and then add this function to your 'handleEventHook'.
focusFollowsTiledOnly :: Event -> X All
focusFollowsTiledOnly e@(CrossingEvent {ev_window = w, ev_event_type = t})
  | isNormalEnter = whenX bothTiled (focus w) >> mempty
  where isNormalEnter   = t == enterNotify && ev_mode e == notifyNormal
        bothTiled       = notFloating w <&&> currentIsTiled
        currentIsTiled  = currentWindow >>= maybe (return True) notFloating
        currentWindow   = gets $ W.peek . windowset
        notFloating w'  = gets $ not . M.member w' . W.floating . windowset
focusFollowsTiledOnly _ = mempty

myEventHook :: Event -> X All
myEventHook = mconcat
    [ ewmhDesktopsEventHook
    , docksEventHook
    , fadeWindowsEventHook
    , focusFollowsTiledOnly
    , fullscreenEventHook
    ]

myConfig dbus = myBaseConfig
    { modMask = Local.modMask
    , borderWidth = 1
    , normalBorderColor = "#FFD12B"
    , focusedBorderColor = "#FF511F"
    , terminal = Local.terminal
    , workspaces = Local.workspaces
    , layoutHook = desktopLayoutModifiers Local.layoutHook
    , keys = Local.keyBindings
    , logHook = Local.logHook dbus
    , handleEventHook = myEventHook
    , manageHook = Local.manageHook
    , startupHook = myStartupHook
    , mouseBindings = myMouseBindings
    }
  where
    mc = myConfig dbus
    myStartupHook = do
        return () >> EZ.checkKeymap mc (Local.emacsKeys mc)
        startupHook myBaseConfig
        -- adjustEventInput
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
