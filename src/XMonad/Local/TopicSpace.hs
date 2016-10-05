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
import XMonad.Local.Config

tmuxProjects :: [WorkspaceId]
tmuxProjects =
    [ "distribution"
    , "docker"
    , "kbcsv"
    , "openshift"
    , "osdocs"
    , "rcs"
    , "xminad"
    , "containers"
    ]

topicDirs :: M.Map WorkspaceId String
topicDirs = M.fromList $
    [ ("distribution", "~/wsp/rh/distribution")
    , ("docker"      , "~/wsp/rh/docker")
    , ("containers"  , "~/wsp/rh/containers")
    , ("docs"        , "~/Documents/doc")
    , ("drive"       , "~/gdrive")
    , ("fedora"      , "~/fedora-scm")
    , ("hdparm"      , "~/fedora-scm/hdparm")
    , ("hwdata"      , "~/wsp/rh/hwdata")
    , ("k8s"         , "~/wsp/rh/kubernetes")
    , ("kbcsv"       , "~/wsp/my/kbcsv")
    , ("mymoney"     , "~/Documents/my-money")
    , ("openshift"   , "~/wsp/rh/openshift-origin")
    , ("osdocs"      , "~/wsp/rh/openshift-docs")
    , ("pdf"         , "~/Documents")
    , ("rcs"         , "~/.rcs")
    , ("rhel"        , "~/rhel-scm")
    , ("scripts"     , "~/wsp/rh/openlmi-scripts")
    , ("tools"       , "~/wsp/rh/openlmi-tools")
    , ("video"       , "~/Videos")
    , ("xminad"      , "~/wsp/my/xminad")
    , ("xmonad"      , "~/wsp/my/xminad")
    ] ++ map (\w -> (w, "~")) homeScoped
  where
    homeScoped =
        [ "admin"
        , "bank"
        , "BG"
        , "calendar"
        , "ciV"
        , "earth"
        , "ebook"
        , "gimp"
        , "graphics"
        , "incognito"
        , "music"
        , "p2p"
        , "remote"
        , "scrum"
        , "web"
        , "witcher"
        ]

topicConfig :: TS.TopicConfig
topicConfig = TS.def
    { TS.topicDirs = topicDirs
    , TS.topicActions = M.fromList $
        [ ("music", spawn "gmpc")
        , ("mail", spawn "thunderbird")
        , ("web", spawn $ browser ++ " --profile-directory=Default")
        , ("firefox", spawn "firefox")
        , ("opera", spawn "opera")
        , ("pdf", spawn "atril")
        , ("chat",
            spawn "pidgin" >>
            spawn "skype" >>
            spawn "telegram-desktop")
        , ("vbox", spawn "VirtualBox")
        , ("gimp", spawn "gimp")
        , ("ebook", spawn "calibre")
        , ("video", spawn "smplayer")
        , ("bank", spawn (browser ++ " --profile-directory=Default" ++
            " https://www.mojebanka.cz/InternetBanking/"))
        , ("p2p", spawn "deluge-gtk")
        , ("hwdata",
              spawnShell Nothing >>
              spawnShellIn "~/fedora-scm/hwdata" Nothing >>
              spawnShellIn "~/rhel-scm/hwdata" Nothing)
        , ("hdparm", spawnShell Nothing >>
              spawnShellIn "~/fedora-scm/hdparm" Nothing >>
              spawnShellIn "~/rhel-scm/hdparm" Nothing)
        , ("k8s",
                spawnShellIn "~/wsp/go/kubernetes" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/wsp/go/kubernetes" (Just "bash --rcfile .bashrc") >>
                spawnShellIn "~/wsp/go/kubernetes" (Just "bash --rcfile .bashrc"))
        , ("scripts", spawnShell Nothing >> spawnShell Nothing)
        , ("ciV", spawn "launch-ciV.sh -m -b")
        , ("scrum", spawn $ browser ++ " --new-window https://bluejeans.com/3046463974/")
        , ("BG", spawn "steam steam://rungameid/228280" >>
                spawn (browser ++ " --new-window http://slovnik.seznam.cz/de-cz/"))
        , ("witcher", spawn "wine 'C:/Program Files (x86)/Steam/Steam.exe' steam://rungameid/20900" >>
                spawn (browser ++ " --new-window http://slovnik.seznam.cz/de-cz/"))
        , ("drive", spawnShell Nothing >> spawnExplorerIn "~/gdrive")
        , ("calendar",
                spawn (browser ++ " --profile-directory=RedHat" ++
                    " --app-id=ejjicmeblgpmajnghnpcppodonldlgfn" ++
                    " --auth-server-whitelist=*.redhat.com") >>
                spawn (browser ++ " --profile-directory=Default" ++
                    " --app-id=ejjicmeblgpmajnghnpcppodonldlgfn"))
        , ("mymoney", spawn "kmymoney")]
        ++ map (\w -> (w, spawnShell Nothing >> spawnShell Nothing))
            [ "remote", "devel", "admin" ]
        ++ map (\w -> (w, spawnTmux w)) tmuxProjects

    , TS.defaultTopicAction = const $ return ()
    , TS.defaultTopic = "dashboard"
    }

workspaces :: [WorkspaceId]
workspaces = ["dashboard", "devel"]
