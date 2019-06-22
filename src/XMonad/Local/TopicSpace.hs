{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

module XMonad.Local.TopicSpace (
      topicConfig
    , topicDirs
    , workspaces
    ) where

import qualified Data.Map                  as M
import           XMonad                    hiding (workspaces)
import qualified XMonad.Actions.TopicSpace as TS
import           XMonad.Util.Run

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
    , "nixos"
    , "openshift"
    , "osdocs"
    , "osregistry"
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
    , ("nixos"       , "~/wsp/nixos")
    , ("openshift"   , "~/wsp/rh/openshift-origin")
    , ("osansible"   , "~/wsp/rh/openshift-ansible")
    , ("osdocs"      , "~/wsp/rh/openshift-docs")
    , ("osregistry"   , "~/wsp/rh/image-registry")
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
    ] ++ map (, "~") homeScoped

homeScoped ∷ [String]
homeScoped =
    [ "admin"
    , "bank"
    , "BG"
    , "calendar"
    , "chat"
    , "ciV"
    , "earth"
    , "ebook"
    , "gimp"
    , "gothic"
    , "graphics"
    , "learn"
    , "incognito"
    , "morrowind"
    , "mail"
    , "maps"
    , "music"
    , "p2p"
    , "play"
    , "remote"
    , "rhdocs"
    , "rhcloud"
    , "rhchat"
    , "rmtdesk"
    , "sap"
    , "scrum"
    , "vbox"
    , "virt"
    , "web"
    , "witcher"
    , "wchat"
    , "wmail"
    , "work"
    ]

topicConfig ∷ TS.TopicConfig
topicConfig = TS.def
    { TS.topicDirs = topicDirs
    , TS.topicActions = M.fromList $
        [ ("music",     safeSpawnProg "gmpc"
                     >> spawnInShellIn "~/Music" "ncmpcpp")
        , ("anki",      unsafeSpawn "anki -b ~/Documents/memory/anki -p synchronized")
        , ("panki",     safeSpawnProg "panki")
        , ("rhanki",    safeSpawnProg "rhanki")
        , ("incognito", safeSpawn browser ["-i"])
        , ("mail",      safeSpawnProg "gmail")
        , ("docs",      safeSpawnProg "gdocs"
                     >> safeSpawnProg "gsheets")
        , ("rhdocs",    safeSpawnProg "rhgdocs"
                     >> safeSpawnProg "rhgsheets")
        , ("wmail",     safeSpawnProg "rhgmail"
                     >> safeSpawnProg "sapmail")
        , ("web",       safeSpawnProg browser)
        , ("wchat",     safeSpawnProg "pidgin"
                     >> safeSpawnProg "slack"
                     >> safeSpawnProg "rhchat")
        , ("work",      safeSpawn browser ["RedHat"])
        , ("firefox",   safeSpawnProg "firefox")
        , ("opera",     safeSpawnProg "opera")
        , ("pdf",       safeSpawnProg "atril")
        , ("chat",      safeSpawnProg "telegram-desktop"
                     >> safeSpawnProg "skypeforlinux"
                     >> safeSpawnProg "whatsapp"
                     >> safeSpawnProg "wireweb")
        , ("vbox",      safeSpawnProg "VirtualBox")
        , ("virt",      safeSpawnProg "virt-manager")
        , ("gimp",      safeSpawnProg "gimp")
        , ("ebook",     safeSpawnProg "calibre")
        , ("video",     safeSpawnProg "smplayer")
        , ("p2p",       safeSpawnProg "deluge-gtk")
        , ("hwdata",    spawnShell
                     >> spawnShellIn "~/fedora-scm/hwdata"
                     >> spawnShellIn "~/rhel-scm/hwdata")
        , ("hdparm",    spawnShell
                     >> spawnShellIn "~/fedora-scm/hdparm"
                     >> spawnShellIn "~/rhel-scm/hdparm")
        , ("scripts",   spawnShell
                     >> spawnShell)
        , ("ciV",       safeSpawn "launch-ciV.sh" ["-m", "-b"])
        , ("scrum",     openURL "https://bluejeans.com/3024462685/")
        , ("BG",        safeSpawn "steam" ["steam://rungameid/228280"]
                     >> openURL "http://slovnik.seznam.cz/de-cz/")
        , ("gothic",    spawnSteamGameInWine 65540)
        , ("morrowind", spawnSteamGameInWine 22320)
        , ("witcher",   spawnSteamGameInWine 20900
                     >> openURL "http://slovnik.seznam.cz/de-cz/")
        , ("cloud",     safeSpawnProg "gdrive"
                     >> safeSpawnProg "mega")
        , ("rhcloud",   safeSpawnProg "rhgdrive")
        , ("drive",     spawnShell
                     >> spawnExplorerIn "~/gdrive")
        , ("calendar",  safeSpawnProg "gcalendar"
                     >> safeSpawnProg "rhgcalendar")
        , ("mymoney",   safeSpawnProg "mymoney")
        , ("sap",       safeSpawn browser ["SAP"])]
        ++ map (, spawnShell >> spawnShell)
            [ "remote", "devel", "admin" ]
        ++ map (\w -> (w, spawnTmux w)) tmuxProjects

    , TS.defaultTopicAction = const $ return ()
    , TS.defaultTopic = "dashboard"
    }

workspaces ∷ [WorkspaceId]
workspaces = ["dashboard", "devel"]
