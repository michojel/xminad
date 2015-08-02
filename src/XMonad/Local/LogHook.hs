module XMonad.Local.LogHook (logHook) where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.Map as M
import Data.List
import qualified DBus as D
import qualified DBus.Client as D
import Text.Regex
import Text.Regex.Posix

-- local modules **************************************************************
import XMonad hiding (logHook)
import qualified XMonad.Actions.UpdatePointer as UP
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad as NS
import XMonad.Util.WorkspaceCompare (getSortByIndex)

logHook :: D.Client -> X()
logHook dbus = do
    sorted <- getSortByIndex
    ws <- gets ( map W.tag . sorted . namedScratchpadFilterOutWorkspace
               . W.workspaces . windowset
               )
    DL.dynamicLogWithPP (myPP $ M.fromList $ zip ws ([1..] :: [Integer])) {
        ppOutput = dbusOutput dbus
    }
    currentWorkspaceOnTop
    ewmhDesktopsLogHook
    fadeWindowsLogHook myFadeHook
    UP.updatePointer (UP.Relative 0.9 0.9)

myPP :: Show a => M.Map WorkspaceId a -> PP
myPP wmap = defaultPP
    { ppTitle    = pangoSpan [("foreground", "white"), ("font", "Cantarell 10")] . pangoSanitize
    , ppCurrent  = pangoColor "white" . wrap "[" "]"
                    . pangoSanitize . indexWorkspace
    , ppVisible  = pangoColor "yellow" . wrap "(" ")"
                    . pangoSanitize . indexWorkspace
    , ppHidden   = _hidden . noScratchPad
    , ppUrgent   = pangoColor "#FF0000"
                 . pangoSanitize . indexWorkspace
    , ppLayout   = pangoColor "lightblue" . pangoSanitize . shortenLayout
    , ppSep      = pangoColor "brown" $ pangoSanitize " : "
    , ppWsSep    = " "
    }
  where
    topicLength :: Integer
    topicLength = 3
    _hidden :: String -> String
    _hidden [] = ""
    _hidden x = pangoColor "#9a9a9a" . pangoSanitize
              . _shorten . indexWorkspace $ x

    _shorten :: String -> String
    _shorten ws = let m = ws =~ ("[0-9]+:.{0," ++ show topicLength ++ "}")
                  in if m == "" then ws else m

    indexWorkspace :: WorkspaceId -> WorkspaceId
    indexWorkspace w | w `M.member` wmap = show (wmap M.! w) ++ ":" ++ w
                     | otherwise         = w

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

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal 
                (D.objectPath_ "/org/xmonad/Log")
                (D.interfaceName_ "org.xmonad.Log")
                (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<b>" ++ (pangoSpan [("font", "Cantarell Bold 10")] $ UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoSpan :: [(String, String)] -> String -> String
pangoSpan attrs = wrap left right
  where
    left = "<span " ++ attrstr ++ ">"
    right = "</span>"
    attrstr = intercalate " " $ fmap (\(x, y) -> x ++ "=\"" ++ y ++ "\"") attrs

pangoColor :: String -> String -> String
pangoColor fg = pangoSpan [("foreground", fg)]

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
