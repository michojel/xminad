module XMonad.Local.ManageHook (manageHook) where

import Data.List
import XMonad hiding (manageHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad as NS
import qualified XMonad.StackSet as W

-- local modules **************************************************************
import XMonad.Local.NamedScratchpad
import XMonad.Local.Workspaces

{- note: earlier hooks override later ones -}
manageHook :: ManageHook
manageHook = composeOne (concat
        [ --[manageHook myBaseConfig]
          [checkDock -?> doIgnore]
        , [className =? c -?> doIgnore | c <- myCIgnores]
        , [className =? "Wine" -?> doFloat ]
        , [isFullscreen -?> doMaster <+> doFullFloat]
        , [transience]
        , [isDialog -?> doMaster <+> doCenterFloat]

        , [fmap ("Preferences" `isInfixOf`) title -?> doMaster <+> doCenterFloat]
        , [(className =? "Gimp" <&&> c) -?> h | (c, h) <- gimpManage]
        , [(    className =? "Qjackctl"
           <&&> fmap ("JACK Audio Connection Kit" `isPrefixOf`) title)
            -?> doMaster <+> doFloat]
        , [(className =? "Dia" <&&> role =? "toolbox_window")
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
        , [NS.query c -?> hook c | c <- namedScratchpads]])
        --, [className =? "dzen" -?> transparency 0.4]])
    <+>
    composeOne (concat
        [ [className =? "Dia"          -?> doMyShift "dia"]
        , [className =? c              -?> doMyShift "chat" | c <- myChatClients ]
        , [(className =? "Google-chrome" <&&> title =? "Hangouts") -?> doMyShift "chat"]
        , [className =? c              -?> doMyShift "web"  | c <- myWebBrowsers ]
        , [title =? "ncmpcpp"          -?> doMyShift "music" ]
        , [className =? c              -?> doMyShift "music" | c <- myMusicPlayers ]
        , [className =? c              -?> doMyShift "video" | c <- myVideoPlayers ]
        , [className =? "Evince"       -?> doMyShift "pdf" ]
        , [className =? "Atril"        -?> doMyShift "pdf" ]
        , [className =? "Thunderbird"  -?> doMyShift "mail"]
        , [className =? "California"   -?> doMyShift "calendar"]
        , [className =? "Gimp"         -?> doMyShift "gimp"]
        , [className =? "Virt-manager" -?> doMyShift "virt"]
        , [className =? "VirtualBox"   -?> doMyShift "vbox"]
        , [className =? "Deluge"       -?> doMyShift "p2p"]
        , [className =? "Calibre"      -?> doMyShift "ebook"]
        , [className =? "Squeak"       -?> doMyShift "squeak"]
        , [className =? "Civ5XP"       -?> doMyShift "ciV"]
        , [className =? "Googleearth-bin" -?> doMyShift "earth"]
        -- see http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Hooks-XPropManage.html#xPropManageHook
        ])
  where
    doMaster = doF W.shiftMaster
    doMyShift :: WorkspaceId -> ManageHook
    doMyShift wsp = do
        liftX (newWorkspace wsp)
        doF $ W.greedyView wsp . W.shift wsp
    myTFloats = [ "VLC (XVideo output)"
              , "DownThemAll! - Make Your Selection"
              , "Add Downloads"
              , "Add URL(s)"
              , "Run Application"
              ]
    myCIgnores = [ "Xfce4-notifyd"
                 ]
    myCFloats = [ -- "Gnome-panel"
                  "Close session"
                , "MPlayer"
                , "Wine"
                , "Galculator"
                ]
    myCTFloats = [ ("Skype", "Information")
                 , ("Firefox", "Certificate Manager")
                 , ("processing-app-Base", "Preferences")
                 , ("Thunar", "File Operation Progress")
                 ]
    myCCenterFloats = [ -- "Gnome-tweak-tool"
                        "Xfce4-notes"
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
    myWebBrowsers  = [ -- "Firefox"
          "Chromium-browser"
        , "Google-chrome"
        ]
    myMusicPlayers = ["ncmpcpp", "Sonata", "Rhythmbox", "Gmpc"]
    myVideoPlayers = ["MPlayer", "Vlc", "Smplayer"]
    gimpManage = [ ( role =? "gimp-toolbox" <||> role =? "gimp-image-window"
                   , ask >>= doF . W.sink)
                 , (role =? "gimp-image-merge-layers", doCenterFloat)
                 , (title =? "Scale Image", doCenterFloat)
                 , (title =? "Export File", doCenterFloat)
                 , (fmap ("Save as" `isPrefixOf`) title, doCenterFloat)
                 ]

    role = stringProperty "WM_WINDOW_ROLE"
