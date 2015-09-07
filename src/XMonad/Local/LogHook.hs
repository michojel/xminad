module XMonad.Local.LogHook (logHook) where

import qualified Data.Map.Strict as M
import System.IO -- (Handle, hPutStrLn)
import Text.Regex
import Text.Regex.Posix

-- local modules **************************************************************
import XMonad hiding (logHook)
import qualified XMonad.Actions.UpdatePointer as UP
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import qualified XMonad.StackSet as W
import qualified XMonad.Util.NamedScratchpad as NS
import XMonad.Util.WorkspaceCompare (getSortByIndex)

logHook :: Handle -> X()
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
    UP.updatePointer (UP.Relative 0.9 0.9)

myPP :: M.Map WorkspaceId Int -> PP
myPP wmap = xmobarPP
    { ppTitle    = xmobarColor "white" "" . xmobarSanitize . shorten 120
    , ppCurrent  = xmobarColor "white" "" . wrap "[" "]"
                    . indexWorkspace False
    , ppVisible  = xmobarColor "yellow" "" . wrap "(" ")"
                    . indexWorkspace False
    , ppHidden   = hidden . noScratchPad
    , ppUrgent   = xmobarColor "red" "black" . indexWorkspace False
    , ppLayout   = xmobarColor "lightblue" "" . aWrap "space" . xmobarSanitize . shortenLayout
    , ppSep      = xmobarColor "brown" "" $ xmobarSanitize " : "
    , ppWsSep    = " "
    }
  where
    topicLength :: Int
    topicLength = 3

    hidden :: String -> String
    hidden [] = ""
    hidden x = xmobarColor "#9a9a9a" "" $ indexWorkspace True x

    indexWorkspace :: Bool -> WorkspaceId -> WorkspaceId
    indexWorkspace shorten' w
        | w `M.member` wmap = clickable index (show index ++ ":" ++ toName shorten' w)
        | otherwise         = clickable index w
        where
            index :: Int
            index = wmap M.! w

    toName :: Bool -> WorkspaceId -> String
    toName True  = take topicLength
    toName False = id

    clickable :: Int -> String -> String
    clickable index | index == 10 = aWrap "0"
                    | index < 10  = aWrap (show index)
                    | otherwise   = xmobarSanitize

    aWrap :: String -> String -> String
    aWrap key w = "<action=`xdotool key super+" ++ key ++
                    "` button=1>" ++ xmobarSanitize w ++ "</action>"

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

myFadeHook :: FadeHook
myFadeHook = composeAll [ opaque
                        , isUnfocused         --> transparency 0.2
                        , doNotFadeOutWindows --> opaque
                        ]

doNotFadeOutWindows :: Query Bool
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

xmobarSanitize :: String -> String
xmobarSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize x    xs = x:xs
