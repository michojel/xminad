{-# LANGUAGE UnicodeSyntax #-}

module XMonad.Local.LogHook (logHook) where

import qualified Data.Map.Strict                    as M
import           System.IO
import           Text.Regex
import           Text.Regex.Posix

import           XMonad                             hiding (logHook)
import qualified XMonad.Actions.UpdatePointer       as UP
import           XMonad.Hooks.CurrentWorkspaceOnTop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeWindows
import qualified XMonad.StackSet                    as W
import qualified XMonad.Util.NamedScratchpad        as NS
import           XMonad.Util.WorkspaceCompare       (getSortByIndex)

logHook ∷ Handle → X()
logHook xmobarHandle = do
    sorted <- getSortByIndex
    ws <- gets $ map W.tag . sorted . NS.namedScratchpadFilterOutWorkspace
               . W.workspaces . windowset
    dynamicLogWithPP (myPP $ M.fromList $ zip ws ([1..] :: [Int])) {
        ppOutput = hPutStrLn xmobarHandle
    }
    currentWorkspaceOnTop
    ewmhDesktopsLogHook
    fadeWindowsLogHook myFadeHook
    UP.updatePointer (0.9, 0.9) (0.1, 0.1)

myPP ∷ M.Map WorkspaceId Int → PP
myPP wmap = xmobarPP
    { ppTitle    = xmobarColor "white" "" . xmobarSanitize . shorten 120
    , ppCurrent  = xmobarColor "white" "" . wrap "[" "]"
                    . indexWorkspace False
    , ppVisible  = xmobarColor "yellow" "" . wrap "(" ")"
                    . indexWorkspace False
    , ppHidden   = hidden . noScratchPad
    , ppUrgent   = xmobarColor "red" "black" . indexWorkspace False
    , ppLayout   = xmobarColor "lightblue" "" . mkClickAction "super+space" . shortenLayout
    , ppSep      = xmobarColor "brown" "" $ xmobarSanitize " : "
    , ppWsSep    = " "
    }
  where
    topicLength ∷ Int
    topicLength = 3

    hidden ∷ String → String
    hidden [] = ""
    hidden x  = xmobarColor "#9a9a9a" "" $ indexWorkspace True x

    indexWorkspace ∷ Bool → WorkspaceId → WorkspaceId
    indexWorkspace shorten' w
        | w `M.member` wmap = mkInteractiveWorkspace index (show index ++ ":" ++ toName shorten' w)
        | otherwise         = toName shorten' w
        where
            index ∷ Int
            index = wmap M.! w

    toName ∷ Bool → WorkspaceId → String
    toName True  = take topicLength
    toName False = id

    mkInteractiveWorkspace ∷ Int → String → String
    mkInteractiveWorkspace desktopIndex workspace = "<action=`xdotool set_desktop "
        ++ show desktopIndex ++ "` button=1>"
        ++ xmobarSanitize workspace ++ "</action>"

    mkClickAction ∷ String → String → String
    mkClickAction key label = "<action=`xdotool key " ++ key
        ++ "` button=1>" ++ xmobarSanitize label ++ "</action>"

    noScratchPad ws | ws =~ "^NSP(:[0-9]+)?$" = ""
                    | otherwise               = ws

    shortenLayout = shortenLayout' [
          ("^Tabbed\\s+(.*)", "T:\\1")
        , ("\\bThreeCol\\b", "OHH")
        , ("\\bThreeColMid\\b", "HOH")
        , ("\\bReflectX\\s+(.*)", "RX:\\1")
        , ("\\bReflectY\\s+(.*)", "RY:\\1")
        , ("\\bMirror\\s+(.*)", "M:\\1")
        , ("\\bResizableTall\\b", "OH")
        ]
    shortenLayout' [] s = s
    shortenLayout' ((reg, repl):xs) s = shortenLayout' xs
                                      $ subRegex (mkRegex reg) s repl

myFadeHook ∷ FadeHook
myFadeHook = composeAll [ opaque
                        , isUnfocused         --> transparency 0.2
                        , doNotFadeOutWindows --> opaque
                        ]

doNotFadeOutWindows ∷ Query Bool
doNotFadeOutWindows =
    className =? "xine" <||>
    className =? "MPlayer" <||>
    className =? "Smplayer" <||>
    className =? "Vlc" <||>
    className =? "Firefox" <||>
    className =? "Opera" <||>
    className =? "Shiretoko" <||>
    className =? "VirtualBox" <||>
    className =? "Namoroka" <||>
    className =? "Navigator" <||>
    className =? "Chromium" <||>
    className =? "Google-chrome" <||>
    className =? "Civ5XP" <||>
    className =? "BaldursGate" <||>
    title     =? "VLC (XVideo output)"

xmobarSanitize ∷ String → String
xmobarSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize x    xs = x:xs
