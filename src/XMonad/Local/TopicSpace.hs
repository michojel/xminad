{-# LANGUAGE UnicodeSyntax #-}

module XMonad.Local.TopicSpace (
      topicConfig
    , topicDirs
    , workspaces
    ) where

import qualified Data.Map                  as M
import           XMonad                    hiding (workspaces)
import qualified XMonad.Actions.TopicSpace as TS

-- local modules **************************************************************
import           XMonad.Local.Actions
import           XMonad.Local.Config

tmuxProjects ∷ [WorkspaceId]
tmuxProjects =
    [ "adminscripts"
    , "aoscdjobs"
    , "containers"
    , "distribution"
    , "docker"
    , "kbcsv"
    , "k8s"
    , "k8sextstg"
    , "openshift"
    , "osdocs"
    , "rcs"
    , "sapvora"
    , "xminad"
    ]

topicDirs ∷ M.Map WorkspaceId String
topicDirs = M.fromList $
    [ ("adminscripts", "~/wsp/my/adminscripts")
    , ("anki"        , "~/Documents/memory/anki")
    , ("distribution", "~/wsp/rh/distribution")
    , ("docker"      , "~/wsp/rh/docker")
    , ("containers"  , "~/wsp/rh/containers")
    , ("docs"        , "~/Documents/doc")
    , ("drive"       , "~/gdrive")
    , ("fedora"      , "~/fedora-scm")
    , ("hdparm"      , "~/fedora-scm/hdparm")
    , ("hwdata"      , "~/wsp/rh/hwdata")
    , ("k8s"         , "~/wsp/rh/kubernetes")
    , ("k8sextstg"   , "~/wsp/rh/k8s-external-storage")
    , ("kbcsv"       , "~/wsp/my/kbcsv")
    , ("mymoney"     , "~/Documents/my-money")
    , ("openshift"   , "~/wsp/rh/openshift-origin")
    , ("osansible"   , "~/wsp/rh/openshift-ansible")
    , ("osdocs"      , "~/wsp/rh/openshift-docs")
    , ("panki"       , "~/Documents/memory/anki")
    , ("rhanki"      , "~/Documents/memory/anki")
    , ("pdf"         , "~/Documents")
    , ("rcs"         , "~/.rcs")
    , ("rhel"        , "~/rhel-scm")
    , ("scripts"     , "~/wsp/rh/openlmi-scripts")
    , ("tools"       , "~/wsp/rh/openlmi-tools")
    , ("video"       , "~/Videos")
    , ("xminad"      , "~/wsp/my/xminad")
    , ("xmonad"      , "~/wsp/my/xminad")
    ] ++ map (\w -> (w, "~")) homeScoped

homeScoped ∷ [String]
homeScoped =
    [ "admin"
    , "bank"
    , "BG"
    , "calendar"
    , "ciV"
    , "earth"
    , "ebook"
    , "gimp"
    , "gothic"
    , "graphics"
    , "incognito"
    , "morrowind"
    , "music"
    , "p2p"
    , "remote"
    , "rmtdesk"
    , "scrum"
    , "virt"
    , "web"
    , "witcher"
    , "work"
    ]

topicConfig ∷ TS.TopicConfig
topicConfig = TS.def
    { TS.topicDirs = topicDirs
    , TS.topicActions = M.fromList $
        [ ("music", spawn "gmpc" >> spawnShellIn "~/Music" (Just "ncmpcpp"))
        , ("anki", spawn "anki -b ~/Documents/memory/anki -p synchronized")
        , ("panki", spawn "panki")
        , ("rhanki", spawn "rhanki")
        , ("incognito", spawn $ browser ++ " -i")
        , ("mail", spawn "thunderbird")
        , ("web", spawn browser)
        , ("work", spawn $ browser ++ " RedHat")
        , ("firefox", spawn "firefox")
        , ("opera", spawn "opera")
        , ("pdf", spawn "atril")
        , ("chat",
            spawn "wire-desktop" >>
            spawn "telegram-desktop")
        , ("vbox", spawn "VirtualBox")
        , ("virt", spawn "virt-manager")
        , ("gimp", spawn "gimp")
        , ("ebook", spawn "calibre")
        , ("video", spawn "smplayer")
        , ("bank", spawn (browser ++ " -w https://www.mojebanka.cz/InternetBanking/"))
        , ("p2p", spawn "deluge-gtk")
        , ("hwdata",
              spawnShell Nothing >>
              spawnShellIn "~/fedora-scm/hwdata" Nothing >>
              spawnShellIn "~/rhel-scm/hwdata" Nothing)
        , ("hdparm", spawnShell Nothing >>
              spawnShellIn "~/fedora-scm/hdparm" Nothing >>
              spawnShellIn "~/rhel-scm/hdparm" Nothing)
        , ("scripts", spawnShell Nothing >> spawnShell Nothing)
        , ("ciV", spawn "launch-ciV.sh -m -b")
        , ("scrum", spawn $ browser ++ " https://bluejeans.com/3024462685/")
        , ("BG", spawn "steam steam://rungameid/228280" >>
                spawn (browser ++ " -n http://slovnik.seznam.cz/de-cz/"))
        , ("gothic", spawn "wine 'C:/Program Files (x86)/Steam/Steam.exe' steam://rungameid/65540")
        , ("morrowind", spawn "wine 'C:/Program Files (x86)/Steam/Steam.exe' steam://rungameid/22320")
        , ("witcher", spawn "wine 'C:/Program Files (x86)/Steam/Steam.exe' steam://rungameid/20900" >>
                spawn (browser ++ " -n http://slovnik.seznam.cz/de-cz/"))
        , ("drive", spawnShell Nothing >> spawnExplorerIn "~/gdrive")
        , ("calendar", spawn "korganizer")
        , ("mymoney", spawn "mymoney")
        , ("rmtdesk", spawn $ browser ++ " -o Default --app-id=" ++ remoteDesktopAppID)]
        ++ map (\w -> (w, spawnShell Nothing >> spawnShell Nothing))
            [ "remote", "devel", "admin" ]
        ++ map (\w -> (w, spawnTmux w)) tmuxProjects

    , TS.defaultTopicAction = const $ return ()
    , TS.defaultTopic = "dashboard"
    }

workspaces ∷ [WorkspaceId]
workspaces = ["dashboard", "devel"]
