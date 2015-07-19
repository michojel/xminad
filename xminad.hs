{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

import qualified Codec.Binary.UTF8.String as UTF8
--import Control.Exception
import qualified Data.Map as M
import Data.List
import Data.Monoid
import qualified DBus as D
import qualified DBus.Client as D
import Text.Regex
import Text.Regex.Posix
--import System.Exit
--import System.IO

import XMonad
--import XMonad.Actions.UpdateFocus
import qualified XMonad.Actions.FlexibleResize as FlexR
import qualified XMonad.Actions.UpdatePointer as UP
import XMonad.Config.Desktop
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import qualified XMonad.Util.EZConfig as EZ
import XMonad.Util.NamedScratchpad as NS
--import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByIndex)

-- local modules **************************************************************
import qualified XMonad.Local.Config as Local
import qualified XMonad.Local.Keys as Local
import qualified XMonad.Local.Layout as Local
import qualified XMonad.Local.ManageHook as Local
import qualified XMonad.Local.TopicSpace as Local

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



myFadeHook :: FadeHook
myFadeHook = composeAll [ opaque
                        , isUnfocused         --> transparency 0.2
                        , doNotFadeOutWindows --> opaque
                        ]

--myLogHook :: Handle -> X ()
myLogHook dbus = do
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

--pangoColor2 :: String -> String -> String -> String
--pangoColor2 fg bg = pangoSpan [("foreground", fg), ("background", bg)]

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

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
    , logHook = myLogHook dbus
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
