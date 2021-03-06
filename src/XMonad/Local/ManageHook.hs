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
       , [appName =? "steam.exe" -?> doIgnore]
       , [transience]
       , [className =? "lxqt-notificationd" -?> doFloat]
       , [className =? c -?> doIgnore | c <- myCIgnores]
       , [(matchChrome <&&> appName =? tabsOutlinerAppName) -?> doTOFloat]
       , [definiteToMaybe $ composeAll [floatManageHook, shiftManageHook]]
       ])]
  where
    doTOFloat ∷ ManageHook
    doTOFloat = doRectFloat tabsOutlinerFloatRect

    myCIgnores = [ "Xfce4-notifyd", "lxqt-notificationd", "xmobar" ]

    alwaysMatch :: Query Bool
    alwaysMatch = fmap ("" `isPrefixOf`) className

    definiteToMaybe :: ManageHook -> MaybeManageHook
    definiteToMaybe m = alwaysMatch -?> m

floatManageHook :: ManageHook
floatManageHook = composeOne (concat
    [ [appName =? "GOTHIC.EXE" -?> doMyShift "gothic" <+> fullscreenManageHook]
    , [appName =? "Morrowind.exe" <||> title =? "Morrowind" -?> doMyShift "morrowind" <+> fullscreenManageHook]
    , [className =? "Wine" -?> doFloat ]
    , [isFullscreen -?> doMaster <+> doFullFloat]
    , [isDialog -?> doMaster <+> doCenterFloat]
    , [className =? "Anki" <&&> title =? "Notiztyp hinzufügen" -?> doFloat]
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
  where
    doMaster = doF W.shiftMaster

    myTFloats = [ "VLC (XVideo output)"
                , "DownThemAll! - Make Your Selection"
                , "Add Downloads"
                , "Add URL(s)"
                , "Run Application"
                ]
    myCFloats = [ "Close session"
                , "MPlayer"
                , "Wine"
                , "Galculator"
                , "Nm-connection-editor"
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

    gimpManage = [ ( windowRole =? "gimp-toolbox" <||> windowRole =? "gimp-image-window"
                   , ask >>= doF . W.sink)
                 , (windowRole =? "gimp-image-merge-layers", doCenterFloat)
                 , (title =? "Scale Image", doCenterFloat)
                 , (title =? "Export File", doCenterFloat)
                 , (fmap ("Save as" `isPrefixOf`) title, doCenterFloat)
                 ]

shiftManageHook :: ManageHook
shiftManageHook = composeOne (concat
    [ [className =? "Dia"             -?> doMyShift "dia"]
    , [className =? c                 -?> doMyShift "chat"  | c <- myChatClients ]
    , [className =? c                 -?> doMyShift "wchat" | c <- myWChatClients ]

    , [matchChrome <&&> appName =? remoteDesktopAppName -?> doMyShift "rmtdesk"]

    , [className =? c                 -?> doMyShift "web"  | c <- myWebBrowsers ]
    , [className =? "Firefox.SAP"     -?> doMyShift "sap"]
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
    , [(className =? "Anki" <&&> fmap ("redhat"       `isSuffixOf`) title) -?> doMyShift "rhanki"]
    , [(className =? "Anki" <&&> fmap ("private"      `isSuffixOf`) title) -?> doMyShift "panki"]
    , [(className =? "Anki" <&&> fmap ("synchronized" `isSuffixOf`) title) -?> doMyShift "anki"]
    , [className =? "Anki"  -?> doMyShift "learn"]
    -- see http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Hooks-XPropManage.html#xPropManageHook

    , [matchChromeApp ["redhat"] app   -?> doMyShift "wmail" | app <- [rhgmailapp, sapmailapp]]
    , [matchChromeApp [] gmailapp -?> doMyShift "mail"]
    , [matchChromeApp [] app           -?> doMyShift "calendar"
            | app <- [gcalendarapp, rhgcalendarapp, sapcalendarapp]]
    , [matchChromeApp [] app          -?> doMyShift "docs"    | app <- [gdocsapp, gsheetsapp]]
    , [matchChromeApp ["redhat"] app  -?> doMyShift "rhdocs"  | app <- [rhgdocsapp, rhgsheetsapp]]
    , [matchChromeApp [] app          -?> doMyShift "cloud"   | app <- [gdriveapp, megaapp]]
    , [matchChromeApp ["redhat"] app  -?> doMyShift "rhcloud" | app <- [rhgdriveapp, sapdriveapp]]
    , [matchChromeApp ["redhat"] app  -?> doMyShift "wchat"   | app <- [rhgmessagesapp, rhgchatapp, sapteams]]
    , [matchSuffixedChrome "nobody"   -?> doMyShift "incognito"]
    , [(matchChrome <&&> title =? "Hangouts") -?> doMyShift "chat"]
    , [matchChromeApp [] app -?> doMyShift "chat"
            | app <- [whatsapp, wireapp, skypeapp, gmessagesapp]]
    , [matchChromeApp [] app -?> doMyShift "play" | app <- [youtubeapp, ytmusicapp]]
    , [matchChromeApp [] duolingoapp -?> doMyShift "learn"]
    , [matchChromeApp [] app -?> doMyShift "maps" | app <- [gmapsapp, mapyapp]]
    , [matchSuffixedChrome "redhat"   -?> doMyShift "work"]
    ])
  where
    myChatClients  = ["Skype", "Empathy", "Wire", "TelegramDesktop"]
    myWChatClients  = ["Xchat", "Hexchat" , "Slack", "Pidgin"]
    myWebBrowsers  = []
    myMusicPlayers = ["ncmpcpp", "Sonata", "Rhythmbox", "Gmpc"]
    myVideoPlayers = ["MPlayer", "Vlc", "Smplayer"]

    duolingoapp    = "crx_aiahmijlpehemcpleichkcokhegllfjl"
    gcalendarapp   = "crx_kjbdgfilnfhdoflbpgamdcdgpehopbep"
    gdocsapp       = "crx_bojccfnmcnekjgjhcaklmcgofnngpjcl"
    gdriveapp      = "crx_lkdnjjllhbbhgjfojnheoooeabjimbka"
    gmailapp       = "crx_pjkljhegncpnkpknbcohdijeoejaedia"
    gmapsapp       = "crx_okmglncioejakncpbchjfnoingecodff"
    gmessagesapp   = "crx_hpfldicfbfomlpcikngkocigghgafkph"
    gsheetsapp     = "crx_lcahnhkcfaikkapifpaenbabamhfnecc"
    mapyapp        = "crx_mnadlckdoclecdmddabnbgjnkfoiddpd"
    megaapp        = "crx_ockmlcfhhimcljikencdeppnoljjjfjk"
    rhgcalendarapp = "crx_kjbdgfilnfhdoflbpgamdcdgpehopbep"
    rhgdocsapp     = "crx_gcefppfnjnmndpknenooeofkfcbakpkp"
    rhgdriveapp    = "crx_lkdnjjllhbbhgjfojnheoooeabjimbka"
    rhgchatapp     = "crx_pommaclcbfghclhalboakcipcmmndhcj"
    rhgmailapp     = "crx_nkcknjnfmnmjahcahhhjgakeikoiomof"
    rhgmessagesapp = "crx_kpbdgbekoclglmjckpbanehbpjnlphkf"
    rhgsheetsapp   = "crx_albjknpbljlpmmpfjicdohagjcifagdi"
    sapcalendarapp = "crx_oeogacjkgmanlfjadbnhngnpbkibgfhj"
    sapdriveapp    = "crx_phgkmbchjgnehfpnmbekcoclneeojdma"
    sapmailapp     = "crx_plnbadkpncgbnekpephdpooeafambhak"
    sapteams       = "crx_jofcjnlbhnljdeapdjgodjlakohpfnjo"
    skypeapp       = "crx_bjdilgfelnbljgdpngladebaeggachpa"
    whatsapp       = "crx_hnpfjngllnobngcgfapefoaidbinmjnm"
    wireapp        = "crx_kfhkficiiapojlgcnbkgacfjmpffgoki"
    youtubeapp     = "crx_blpcfgokakmgnkcojhhkbfbldkacnbeo"
    ytmusicapp     = "crx_eeaafbmfeofhhjpjklgoobllogkjdgap"

doMyShift ∷ WorkspaceId → ManageHook
doMyShift wsp = do
    liftX (newWorkspace wsp)
    doF $ W.greedyView wsp . W.shift wsp

matchSuffixedChrome ∷ String → Query Bool
matchSuffixedChrome suffix = foldr (\a p -> p <||> pcs a) (pcs h) rest
    where
        h      = head chromeClassNames
        rest   = tail chromeClassNames
        pcs cn = className =? (cn ++ "." ++ suffix)

matchChromeApp :: [String] → String → Query Bool
matchChromeApp profiles app = foldr (\a p -> p <||> pcs' a) (pcs' h) rest
    where
        h             = head filters
        rest          = tail filters
        pcs cn suf an
            | suf == ""        = fmap (cn `isPrefixOf`) className <&&> appName =? an
            | suf == "default" = className =? cn <&&> appName =? an
            | otherwise        = className =? (cn ++ "." ++ suf)  <&&> appName =? an
        pcs' (cn, suf, an) = pcs cn suf an
        filters | null profiles = [(cn, "", app)  | cn <- chromeClassNames]
                | otherwise     = [(cn, suf, app) | cn <- chromeClassNames, suf <- profiles]


windowRole ∷ Query String
windowRole = stringProperty "WM_WINDOW_ROLE"
