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
    , ("xmonad"      , "~/wsp/my/xminad")
    , ("xminad"      , "~/wsp/my/xminad")
    , ("video"       , "~/Videos")
    , ("docs"        , "~/Documents/doc")
    , ("pdf"         , "~/Documents")
    , ("graphics"    , "~")
    , ("gimp"        , "~")
    , ("hwdata"      , "~/wsp/rh/hwdata")
    , ("hdparm"      , "~/fedora-scm/hdparm")
    , ("scripts"     , "~/wsp/rh/openlmi-scripts")
    , ("tools"       , "~/wsp/rh/openlmi-tools")
    , ("rhel"        , "~/rhel-scm")
    , ("fedora"      , "~/fedora-scm")
    , ("docker"      , "~/wsp/rh/docker")
    , ("distribution", "~/wsp/rh/distribution")
    , ("openshift"   , "~/wsp/rh/openshift-origin")
    , ("osdocs"      , "~/wsp/rh/openshift-docs")
    , ("k8s"         , "~/wsp/rh/kubernetes")
    , ("drive"       , "~/gdrive")
    ] ++ map (\w -> (w, "~"))
    [ "music", "p2p", "gimp", "graphics"
    , "web", "remote", "earth", "bank", "admin", "ebook"
    , "ciV", "scrum", "BG", "witcher", "calendar"]

topicConfig :: TS.TopicConfig
topicConfig = TS.def
    { TS.topicDirs = topicDirs
    , TS.topicActions = M.fromList $
        [ ("music", spawn "gmpc")
        , ("mail", spawn "thunderbird")
        , ("web", spawn "google-chrome-stable --profile-directory=Default")
        , ("firefox", spawn "firefox")
        , ("opera", spawn "opera")
        , ("pdf", spawn "atril")
        , ("chat",
            spawn "pidgin" >>
            spawn "skype" >>
            spawnShell (Just "mux start irssi"))
        , ("vbox", spawn "VirtualBox")
        , ("gimp", spawn "gimp")
        , ("ebook", spawn "calibre")
        , ("video", spawn "smplayer")
        , ("bank", spawn ("google-chrome-stable --profile-directory=Default" ++
            " https://www.mojebanka.cz/InternetBanking/"))
        , ("p2p", spawn "deluge-gtk")
        , ("hwdata",
              spawnShell Nothing >>
              spawnShellIn "~/fedora-scm/hwdata" Nothing >>
              spawnShellIn "~/rhel-scm/hwdata" Nothing)
        , ("hdparm", spawnShell Nothing >>
              spawnShellIn "~/fedora-scm/hdparm" Nothing >>
              spawnShellIn "~/rhel-scm/hdparm" Nothing)
        , ("docker",
                spawnShellIn "~/wsp/go/docker" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/wsp/go/docker" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/wsp/go/docker" (Just "bash --rcfile .bashrc"))
        , ("openshift",
                spawnShellIn "~/wsp/go/openshift" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/wsp/go/openshift" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/wsp/go/openshift" (Just "bash --rcfile .bashrc"))
        , ("distribution",
                spawnShellIn "~/wsp/go/distribution" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/wsp/go/distribution" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/wsp/go/distribution" (Just "bash --rcfile .bashrc"))
        , ("k8s",
                spawnShellIn "~/wsp/go/kubernetes" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/wsp/go/kubernetes" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/wsp/go/kubernetes" (Just "bash --rcfile .bashrc"))
        , ("scripts", spawnShell Nothing >> spawnShell Nothing)
        , ("ciV", spawn "launch-ciV.sh -m -b")
        , ("scrum", spawn "firefox --new-window https://bluejeans.com/3046463974/")
        , ("BG", spawn "steam steam://rungameid/228280" >>
                spawn "firefox http://slovnik.seznam.cz/de-cz/")
        , ("witcher", spawn "wine 'C:/Program Files (x86)/Steam/Steam.exe' steam://rungameid/20900" >>
                spawn "firefox --new-window http://slovnik.seznam.cz/de-cz/")
        , ("drive", spawnShell Nothing >> spawnExplorerIn "~/gdrive")
        , ("calendar",
                spawn (
                    "google-chrome-stable --profile-directory=RedHat" ++
                    " --app-id=ejjicmeblgpmajnghnpcppodonldlgfn" ++
                    " --auth-server-whitelist=*.redhat.com") >>
                spawn ("google-chrome-stable --profile-directory=Default" ++
                    " --app-id=ejjicmeblgpmajnghnpcppodonldlgfn"))
        ] ++ map (\w -> (w, spawnShell Nothing >> spawnShell Nothing))
        [ "osdocs", "xmonad", "xminad", "remote", "devel", "admin" ]
    , TS.defaultTopicAction = const $ return ()
    , TS.defaultTopic = "dashboard"
    }

workspaces :: [WorkspaceId]
workspaces = ["dashboard", "devel"]
