module XMonad.Local.TopicSpace (
      topicConfig
    , topicDirs
    , workspaces
    ) where

import qualified Data.Map as M
import qualified XMonad as X
import qualified XMonad.Actions.TopicSpace as TS

-- local modules **************************************************************
import XMonad.Local.Actions

topicDirs :: M.Map X.WorkspaceId String
topicDirs = M.fromList $
    [ ("dashboard"   , "~")
    , ("xmonad"      , "~/.xmonad")
    , ("video"       , "~/Documents/movies")
    , ("docs"        , "~/Documents/doc")
    , ("pdf"         , "~/Documents")
    , ("graphics"    , "~")
    , ("gimp"        , "~")
    , ("eclipse"     , "~/workspace")
    , ("yawn"        , "~/workspace/rh/pywbem-yawn")
    , ("openlmi"     , "~/workspace/rh/openlmi-providers")
    , ("cim"         , "/usr/share/mof/cim-current")
    , ("hwdata"      , "~/workspace/rh/hwdata")
    , ("hdparm"      , "~/fedora-scm/hdparm")
    , ("providers"   , "~/workspace/rh/openlmi-providers")
    , ("scripts"     , "~/workspace/rh/openlmi-scripts")
    , ("tools"       , "~/workspace/rh/openlmi-tools")
    , ("rhel"        , "~/rhel-scm")
    , ("fedora"      , "~/fedora-scm")
    , ("docker"      , "~/workspace/rh/docker")
    , ("distribution", "~/workspace/rh/distribution")
    , ("ae"          , "~/workspace/rh/atomic-enterprise")
    , ("aet"         , "~/workspace/rh/atomic-enterprise-training")
    , ("aea"         , "~/workspace/rh/atomic-enterprise-ansible")
    , ("aes"         , "~/workspace/rh/ae-scripts")
    ] ++ map (\w -> (w, "~"))
    [ "music", "p2p", "gimp", "graphics"
    , "web", "remote", "earth", "bank", "admin", "ebook"
    , "ciV", "scrum", "BG"]

topicConfig :: TS.TopicConfig
topicConfig = TS.defaultTopicConfig
    { TS.topicDirs = topicDirs
    , TS.topicActions = M.fromList $
        [ ("music", X.spawn "gmpc")
        -- ("music", X.spawn $ myTerminal ++ " -depth 32 -bg rgba:0000/0000/0000/7777 -fg white -e ncmpcpp")
        , ("mail", X.spawn "thunderbird")
        , ("web", X.spawn "google-chrome")
        , ("firefox", X.spawn "firefox")
        , ("opera", X.spawn "opera")
        , ("pdf", X.spawn "atril")
        , ("chat", X.spawn "xchat" >> X.spawn "pidgin")
        , ("admin", spawnShell Nothing >> spawnShell Nothing)
        , ("virt", X.spawn "virt-manager")
        , ("vbox", X.spawn "VirtualBox")
        , ("gimp", X.spawn "gimp")
        , ("eclipse", X.spawn "eclipse")
        , ("ebook", X.spawn "calibre")
        , ("video", X.spawn "vlc")
        , ("xmonad", spawnShell (Just "vim -S xmonad.vim") >>
              spawnShell Nothing)
        , ("remote", spawnShell Nothing >> spawnShell Nothing)
        , ("devel", spawnShell Nothing >> spawnShell Nothing)
        , ("openlmi",   spawnShell Nothing >> spawnShell Nothing)
        , ("providers", spawnShell Nothing >> spawnShell Nothing)
        , ("cim", spawnShell Nothing >>
              spawnShellIn "/usr/lib/python2.7/site-packages/pywbem" Nothing)
        , ("bank", X.spawn "google-chrome https://www.mojebanka.cz/InternetBanking/")
        , ("p2p", X.spawn "deluge-gtk")
        , ("hwdata",
              spawnShell Nothing >>
              spawnShellIn "~/fedora-scm/hwdata" Nothing >>
              spawnShellIn "~/rhel-scm/hwdata" Nothing)
        , ("hdparm", spawnShell Nothing >>
              spawnShellIn "~/fedora-scm/hdparm" Nothing >>
              spawnShellIn "~/rhel-scm/hdparm" Nothing)
        , ("docker", spawnShell Nothing >> spawnShell Nothing >>
                spawnShellIn "~/workspace/go/docker" (Just "bash --rcfile .bashrc"))
        , ("distribution", spawnShell Nothing >> spawnShell Nothing >>
                spawnShellIn "~/workspace/go/distribution" (Just "bash --rcfile .bashrc"))
        , ("scripts", spawnShell Nothing >> spawnShell Nothing)
        , ("ciV", X.spawn "launch-ciV.sh -m -b")
        , ("scrum", X.spawn "firefox https://bluejeans.com/3046463974/")
        , ("BG", X.spawn "steam steam://rungameid/228280" >>
              X.spawn "firefox http://slovnik.seznam.cz/de-cz/")
        ] ++ map (\w -> (w, spawnShell Nothing >> spawnShell Nothing))
        [ "ae", "aet", "aes", "aea" ]
    , TS.defaultTopicAction = const $ return ()
    , TS.defaultTopic = "dashboard"
    }

workspaces :: [X.WorkspaceId]
workspaces = ["dashboard", "devel"]
