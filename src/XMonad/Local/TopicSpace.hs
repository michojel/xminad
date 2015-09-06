module XMonad.Local.TopicSpace (
      topicConfig
    , topicDirs
    , workspaces
    ) where

import qualified Data.Map as M
import XMonad hiding (workspaces)
import qualified XMonad.Actions.TopicSpace as TS

-- local modules **************************************************************
import XMonad.Local.Actions

topicDirs :: M.Map WorkspaceId String
topicDirs = M.fromList $
    [ ("dashboard"   , "~")
    , ("xmonad"      , "~/my/private/xminad")
    , ("video"       , "~/Documents/movies")
    , ("docs"        , "~/Documents/doc")
    , ("pdf"         , "~/Documents")
    , ("graphics"    , "~")
    , ("gimp"        , "~")
    , ("eclipse"     , "~/my")
    , ("yawn"        , "~/my/rh/pywbem-yawn")
    , ("openlmi"     , "~/my/rh/openlmi-providers")
    , ("cim"         , "/usr/share/mof/cim-current")
    , ("hwdata"      , "~/my/rh/hwdata")
    , ("hdparm"      , "~/fedora-scm/hdparm")
    , ("providers"   , "~/my/rh/openlmi-providers")
    , ("scripts"     , "~/my/rh/openlmi-scripts")
    , ("tools"       , "~/my/rh/openlmi-tools")
    , ("rhel"        , "~/rhel-scm")
    , ("fedora"      , "~/fedora-scm")
    , ("docker"      , "~/my/rh/docker")
    , ("distribution", "~/my/rh/distribution")
    , ("ae"          , "~/my/rh/atomic-enterprise")
    , ("aet"         , "~/my/rh/atomic-enterprise-training")
    , ("aea"         , "~/my/rh/atomic-enterprise-ansible")
    , ("aes"         , "~/my/rh/ae-scripts")
    , ("openshift"   , "~/my/rh/openshift-origin")
    ] ++ map (\w -> (w, "~"))
    [ "music", "p2p", "gimp", "graphics"
    , "web", "remote", "earth", "bank", "admin", "ebook"
    , "ciV", "scrum", "BG"]

topicConfig :: TS.TopicConfig
topicConfig = TS.defaultTopicConfig
    { TS.topicDirs = topicDirs
    , TS.topicActions = M.fromList $
        [ ("music", spawn "gmpc")
        , ("mail", spawn "thunderbird")
        , ("web", spawn "google-chrome")
        , ("firefox", spawn "firefox")
        , ("opera", spawn "opera")
        , ("pdf", spawn "atril")
        , ("chat", spawn "xchat" >> spawn "pidgin")
        , ("admin", spawnShell Nothing >> spawnShell Nothing)
        , ("virt", spawn "virt-manager")
        , ("vbox", spawn "VirtualBox")
        , ("gimp", spawn "gimp")
        , ("eclipse", spawn "eclipse")
        , ("ebook", spawn "calibre")
        , ("video", spawn "vlc")
        , ("xmonad", spawnShell Nothing >> spawnShell Nothing)
        , ("remote", spawnShell Nothing >> spawnShell Nothing)
        , ("devel", spawnShell Nothing >> spawnShell Nothing)
        , ("openlmi",   spawnShell Nothing >> spawnShell Nothing)
        , ("providers", spawnShell Nothing >> spawnShell Nothing)
        , ("cim", spawnShell Nothing >>
              spawnShellIn "/usr/lib/python2.7/site-packages/pywbem" Nothing)
        , ("bank", spawn "google-chrome https://www.mojebanka.cz/InternetBanking/")
        , ("p2p", spawn "deluge-gtk")
        , ("hwdata",
              spawnShell Nothing >>
              spawnShellIn "~/fedora-scm/hwdata" Nothing >>
              spawnShellIn "~/rhel-scm/hwdata" Nothing)
        , ("hdparm", spawnShell Nothing >>
              spawnShellIn "~/fedora-scm/hdparm" Nothing >>
              spawnShellIn "~/rhel-scm/hdparm" Nothing)
        , ("docker",
                spawnShellIn "~/my/go/docker" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/my/go/docker" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/my/go/docker" (Just "bash --rcfile .bashrc"))
        , ("openshift",
                spawnShellIn "~/my/go/openshift" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/my/go/openshift" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/my/go/openshift" (Just "bash --rcfile .bashrc"))
        , ("distribution", spawnShell Nothing >> spawnShell Nothing >>
                spawnShellIn "~/my/go/distribution" (Just "bash --rcfile .bashrc"))
        , ("scripts", spawnShell Nothing >> spawnShell Nothing)
        , ("ciV", spawn "launch-ciV.sh -m -b")
        , ("scrum", spawn "firefox https://bluejeans.com/3046463974/")
        , ("BG", spawn "steam steam://rungameid/228280" >>
              spawn "firefox http://slovnik.seznam.cz/de-cz/")
        ] ++ map (\w -> (w, spawnShell Nothing >> spawnShell Nothing))
        [ "ae", "aet", "aes", "aea" ]
    , TS.defaultTopicAction = const $ return ()
    , TS.defaultTopic = "dashboard"
    }

workspaces :: [WorkspaceId]
workspaces = ["dashboard", "devel"]
