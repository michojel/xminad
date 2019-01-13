{-# LANGUAGE UnicodeSyntax #-}

module XMonad.Local.NamedScratchpad (namedScratchpads) where

import           XMonad.Core                 (ManageHook)
import           XMonad.ManageHook
import qualified XMonad.StackSet             as W
import           XMonad.Util.NamedScratchpad

-- local modules **************************************************************
import           XMonad.Local.Config
import           XMonad.Local.Util

namedScratchpads ∷ [NamedScratchpad]
namedScratchpads =
        [ NS "dictionary" "goldendict" (className =? "GoldenDict") cBigFloat
        , NS "notes" "gvim --role notes ~/notes.txt" (role =? "notes") cFloating
        , NS "charmap" "charmap" (className =? "Gucharmap") cFloating
        , NS "alarm" "alarm-clock-applet"
            (className =? "Alarm-clock-applet") cFloating
        , stns "htop" "htop" cTopFloat
        , stns "calculator" "python" cFloating
        , stns "guake" "tmux -c 'tmuxinator start guake'" cBottomFloat
        , stns "man-browser" "tmux -c 'tmuxinator start man-browser'"  cTopFloat
        , NS "volctl" "pavucontrol" (className =? "pavuctonrol-qt") cFloating
        , NS "tabsoutliner" (browser ++ " Default --app-id=" ++ tabsOutlinerAppID)
           (matchChrome <&&> appName =? tabsOutlinerAppName) cBigFloat
        ]
    where
        role = stringProperty "WM_WINDOW_ROLE"
        cFloating = customFloating $ W.RationalRect (1/3) (1/9) (1/3) (1/3)
        cBigFloat = customFloating tabsOutlinerFloatRect
        cTopFloat = customFloating $ W.RationalRect (1/5) (1/32) (3/5) (1/2)
        cBottomFloat = customFloating $ W.RationalRect (1/5) (6/11) (3/5) (4/11)

        stns ∷ String → String → XMonad.Core.ManageHook → NamedScratchpad
        stns sname scmd = NS sname
            (terminal ++ " -c st-" ++ sname ++ " -n st-" ++ sname ++ " " ++ scmd)
            (appName =? ("st-" ++ sname) <||> className =? ("st-" ++ sname))
