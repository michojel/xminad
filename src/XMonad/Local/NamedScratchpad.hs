module XMonad.Local.NamedScratchpad (namedScratchpads) where

import Data.String.Utils (startswith)
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

-- local modules **************************************************************
import XMonad.Local.Config

namedScratchpads :: [NamedScratchpad]
namedScratchpads =
        [ NS "htop" (terminal ++ " -t htop -e htop") (title =? "htop")
            cTopFloat
        , NS "stardict" "stardict" (className =? "Stardict") cFloating
        , NS "notes" "gvim --role notes ~/notes.txt" (role =? "notes")
            cFloating
        , NS "charmap" "charmap" (className =? "Gucharmap") cFloating
        , NS "alarm" "alarm-clock-applet"
             (className =? "Alarm-clock-applet") cFloating
        , NS "calculator" (terminal ++ " -e python --title PCalculator")
                           (title =? "PCalculator") cFloating
        , NS "volctl" "mate-volume-control" (className =? "Mate-volume-control") cFloating
        , NS "guake" (terminal ++ " --window-with-profile=Guake-normal --tab-with-profile=Guake-root")
             (className =? "Mate-terminal" <&&> (startsWith title "Guake")) cBottomFloat 
        ]
    where
        role = stringProperty "WM_WINDOW_ROLE"
        cFloating = customFloating $ W.RationalRect (1/3) (1/9) (1/3) (1/3)
        cTopFloat = customFloating $ W.RationalRect (1/5) (1/32) (3/5) (1/2)
        cBottomFloat = customFloating $ W.RationalRect (1/5) (5/8) (3/5) (3/8)
        startsWith q x = fmap (startswith x) q
