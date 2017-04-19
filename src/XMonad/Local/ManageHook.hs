{-# LANGUAGE UnicodeSyntax #-}

module XMonad.Local.ManageHook (
      manageHook
    ) where

import           Data.List
import           XMonad                       hiding (manageHook)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Fullscreen
import qualified XMonad.StackSet              as W
import           XMonad.Util.NamedScratchpad  as NS

-- local modules **************************************************************
import           XMonad.Local.Config
import           XMonad.Local.NamedScratchpad
import           XMonad.Local.Util
import           XMonad.Local.Workspaces

{- note: earlier hooks have higher priority -}
manageHook ∷ ManageHook
manageHook = composeAll
     [ composeOne [NS.query c -?> hook c | c <- namedScratchpads]
     , composeOne (concat
        [ [checkDock -?> doIgnore]
        , [(matchChrome <&&> appName =? tabsOutlinerAppName) -?> doTOFloat]
        , [className =? c -?> doIgnore | c <- myCIgnores]
        , [appName =? "GOTHIC.EXE" -?> doMyShift "gothic" <+> fullscreenManageHook]
        , [appName =? "Morrowind.exe" <||> title =? "Morrowind" -?> doMyShift "morrowind" <+> fullscreenManageHook]
        , [className =? "Wine" -?> doFloat ]
        , [isFullscreen -?> doMaster <+> doFullFloat]
        , [transience]
        , [isDialog -?> doMaster <+> doCenterFloat]
        , [fmap ("Preferences" `isInfixOf`) title -?> doMaster <+> doCenterFloat]
        , [(className =? "Gimp" <&&> c) -?> h | (c, h) <- gimpManage]
        , [(    className =? "Qjackctl"
           <&&> fmap ("JACK Audio Connection Kit" `isPrefixOf`) title)
            -?> doMaster <+> doFloat]
        , [(className =? "Dia" <&&> windowRole =? "toolbox_window")
            -?> doMaster <+> doFloat]
        , [ className =? c <&&> title =? t -?> doMaster <+> doFloat
          | (c, t) <- myCTFloats ]
        , [className =? c -?> doMaster <+> doCenterFloat | c <- myCCenterFloats]
        , [className =? "Virt-manager" <&&> title =? "New VM"
          -?> doMaster <+> doCenterFloat]
        , [className =? c -?> doMaster <+> doFloat | c <- myCFloats ]
        , [title =? t -?> doMaster <+> doFloat | t <- myTFloats ]
        , [className =? "BaldursGate" -?> doMyShift "BG" <+> doMaster]
        , [className =? "witcher.exe" -?> doMyShift "witcher" <+> doMaster]
        ])
    , composeOne (concat
        [ [className =? "Dia"             -?> doMyShift "dia"]
        , [className =? c                 -?> doMyShift "chat" | c <- myChatClients ]
        , [matchChrome <&&> appName =? remoteDesktopAppName -?> doMyShift "rmtdesk"]
        , [matchSuffixedChrome "redhat"   -?> doMyShift "work"]
        , [matchSuffixedChrome "nobody"   -?> doMyShift "incognito"]
        , [(matchChrome <&&> title =? "Hangouts") -?> doMyShift "chat"]
        , [className =? c                 -?> doMyShift "web"  | c <- myWebBrowsers ]
        , [title =? "ncmpcpp"             -?> doMyShift "music" ]
        , [className =? c                 -?> doMyShift "music" | c <- myMusicPlayers ]
        , [className =? c                 -?> doMyShift "video" | c <- myVideoPlayers ]
        , [className =? "Evince"          -?> doMyShift "pdf"]
        , [className =? "Atril"           -?> doMyShift "pdf"]
        , [className =? "Thunderbird"     -?> doMyShift "mail"]
        , [className =? "California"      -?> doMyShift "calendar"]
        , [className =? "korganizer"      -?> doMyShift "calendar"]
        , [className =? "Gimp"            -?> doMyShift "gimp"]
        , [className =? "Virt-manager"    -?> doMyShift "virt"]
        , [className =? "VirtualBox"      -?> doMyShift "vbox"]
        , [className =? "Deluge"          -?> doMyShift "p2p"]
        , [className =? "Calibre"         -?> doMyShift "ebook"]
        , [className =? "Squeak"          -?> doMyShift "squeak"]
        , [className =? "Civ5XP"          -?> doMyShift "ciV"]
        , [className =? "Googleearth-bin" -?> doMyShift "earth"]
        -- see http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Hooks-XPropManage.html#xPropManageHook
        ])
    ]
  where
    doMaster = doF W.shiftMaster

    doMyShift ∷ WorkspaceId → ManageHook
    doMyShift wsp = do
        liftX (newWorkspace wsp)
        doF $ W.greedyView wsp . W.shift wsp

    doTOFloat ∷ ManageHook
    doTOFloat = doRectFloat tabsOutlinerFloatRect

    myTFloats = [ "VLC (XVideo output)"
                , "DownThemAll! - Make Your Selection"
                , "Add Downloads"
                , "Add URL(s)"
                , "Run Application"
                ]
    myCIgnores = [ "Xfce4-notifyd"
                 ]
    myCFloats = [ "Close session"
                , "MPlayer"
                , "Wine"
                , "Galculator"
                ]
    myCTFloats = [ ("Skype", "Information")
                 , ("Firefox", "Certificate Manager")
                 , ("processing-app-Base", "Preferences")
                 , ("Thunar", "File Operation Progress")
                 ]
    myCCenterFloats = [ "Xfce4-notes"
                      , "Gcolor2"
                      , "Gcr-prompter"
                      , "Xfce4-appfinder"
                      , "Xmessage"
                      , "Pavucontrol"
                      , "Xfrun4"
                      , "Xfce4-settings-manager"
                      , "Xfce4-panel"
                      , "Alarm-clock-applet"
                      ]
    myChatClients  = ["Pidgin", "Xchat", "Skype", "Empathy", "Hexchat"]
    myWebBrowsers  = []
    myMusicPlayers = ["ncmpcpp", "Sonata", "Rhythmbox", "Gmpc"]
    myVideoPlayers = ["MPlayer", "Vlc", "Smplayer"]

    gimpManage = [ ( windowRole =? "gimp-toolbox" <||> windowRole =? "gimp-image-window"
                   , ask >>= doF . W.sink)
                 , (windowRole =? "gimp-image-merge-layers", doCenterFloat)
                 , (title =? "Scale Image", doCenterFloat)
                 , (title =? "Export File", doCenterFloat)
                 , (fmap ("Save as" `isPrefixOf`) title, doCenterFloat)
                 ]

matchSuffixedChrome ∷ String → Query Bool
matchSuffixedChrome suffix = foldr (\a p -> p <||> pcs a) (pcs h) rest
    where
        h = head chromeClassNames
        rest = tail chromeClassNames
        pcs cn = className =? (cn ++ "." ++ suffix)

windowRole ∷ Query String
windowRole = stringProperty "WM_WINDOW_ROLE"
