module XMonad.Local.Mouse where

import qualified Data.Map as M

import XMonad
import qualified XMonad.Actions.FlexibleResize as FlexR
import qualified XMonad.StackSet as W

-- Mouse bindings: default actions bound to mouse events
mouseBindings :: XConfig Layout
                -> M.Map (ButtonMask, Button) (Window -> X())
mouseBindings (XConfig {XMonad.modMask = mm}) = M.fromList
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
