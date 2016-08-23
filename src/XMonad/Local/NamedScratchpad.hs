module XMonad.Local.NamedScratchpad (namedScratchpads) where

import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

-- local modules **************************************************************
import XMonad.Local.Config

namedScratchpads :: [NamedScratchpad]
namedScratchpads =
        [ NS "htop" (terminal ++ " -t htop -e htop") (title =? "htop")
            cTopFloat
        , NS "stardict" "qstardict" (className =? "Qstardict") cFloating
        , NS "notes" "gvim --role notes ~/notes.txt" (role =? "notes") cFloating
        , NS "charmap" "charmap" (className =? "Gucharmap") cFloating
        , NS "alarm" "alarm-clock-applet"
             (className =? "Alarm-clock-applet") cFloating
        , NS "calculator" (terminal ++ " -e python --title PCalculator")
                           (title =? "PCalculator") cFloating
        , NS "volctl" "mate-volume-control" (className =? "Mate-volume-control") cFloating
        , NS "guake" (terminal ++ " -c st-guake tmux -c 'tmuxinator start guake'")
             (appName =? "st-guake" <||> className =? "st-guake") cBottomFloat 
        , NS "tabsoutliner" ("google-chrome-stable --profile-directory=Default" ++
                " --app-id=" ++ tabsOutlinerAppID)
             (className =? "google-chrome" <&&> appName =? tabsOutlinerAppName) cBigFloat
        ]
    where
        role = stringProperty "WM_WINDOW_ROLE"
        cFloating = customFloating $ W.RationalRect (1/3) (1/9) (1/3) (1/3)
        cBigFloat = customFloating $ tabsOutlinerFloatRect
        cTopFloat = customFloating $ W.RationalRect (1/5) (1/32) (3/5) (1/2)
        cBottomFloat = customFloating $ W.RationalRect (1/5) (6/11) (3/5) (4/11)

