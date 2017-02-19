{-# LANGUAGE UnicodeSyntax #-}
module XMonad.Local.EventHook (eventHook) where

import           Data.Monoid

import           XMonad
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.ManageDocks
import qualified XMonad.Util.Loggers.NamedScratchpad as NS

import qualified XMonad.Local.NamedScratchpad        as Local
import qualified XMonad.Local.Operations             as Local

eventHook ∷ Event → X All
eventHook = mconcat
    [ ewmhDesktopsEventHook
    , NS.nspTrackHook Local.namedScratchpads
    , docksEventHook
    , fadeWindowsEventHook
    , fullscreenEventHook
    , xminadRestartEventHook
    ]

xminadRestartEventHook ∷ Event → X All
xminadRestartEventHook = handle

handle ∷ Event → X All
handle e@ClientMessageEvent { ev_message_type = mt } = do
    a <- getAtom "XMONAD_RESTART"
    if mt == a
        then Local.restart >> return (All False)
        else broadcastMessage e >> return (All True)
handle e = broadcastMessage e >> return (All True)
