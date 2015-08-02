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
    , focusFollowsTiledOnly
    , fullscreenEventHook
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
