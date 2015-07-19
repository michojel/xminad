module XMonad.Local.Keys (
      emacsKeys
    , keyBindings
    , modMask
    ) where

import Control.Monad
import Data.Either.Utils
import qualified Data.Map as M
import Data.Maybe
import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPD
import qualified XMonad as X
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Actions.DwmPromote as DwmP
import XMonad.Actions.GridSelect
import qualified XMonad.Actions.Submap as SUB
import qualified XMonad.Actions.TopicSpace as TS
import XMonad.Actions.Volume
import qualified XMonad.Actions.WithAll as WithAll
import XMonad.Hooks.ManageDocks
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Minimize
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.Reflect
import XMonad.Layout.SubLayouts
import qualified XMonad.Layout.TopicDir as TD
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import qualified XMonad.Prompt.Shell as Shell
import qualified XMonad.Prompt.Ssh as PSsh
import qualified XMonad.StackSet as W
import qualified XMonad.Util.EZConfig as EZ
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (getSortByIndex)

-- local modules **************************************************************
import qualified XMonad.Local.Actions as Local
import XMonad.Local.Config
import XMonad.Local.GridSelect as Local
import XMonad.Local.NamedScratchpad
import XMonad.Local.TopicSpace
import XMonad.Local.Workspaces as Local

modMask :: X.KeyMask
modMask = X.mod4Mask
modm :: String
modm = "M4"

keyBindings :: X.XConfig l -> M.Map (X.KeyMask, X.KeySym) (X.X())
keyBindings conf = EZ.mkKeymap conf $ emacsKeys conf

emacsKeys :: X.XConfig l -> [(String, X.X())]
emacsKeys = \conf -> map prefix (emacsKeys' conf) ++ keys
  where
    prefix :: (String, a) -> (String, a)
    prefix (k, a) = (modm ++ "-" ++ k, a)

    emacsKeys' :: X.XConfig l -> [(String, X.X())]
    emacsKeys' conf = [
               -- Applications
              (";", Local.spawnShell Nothing)
            , ("S-;", Local.spawnExplorer)
            , ("S-.", namedScratchpadAction namedScratchpads  "guake")
            , ("p", Shell.shellPrompt xpConfig)
            , ("S-p", Local.mateRun)

              -- Layouts
            , ("<Space>", X.sendMessage X.NextLayout)
            , ("C-<Space>", SUB.submap $ EZ.mkKeymap conf $ concat
                [ [(k, a), (modm ++ "-C-" ++ k, a)]
                | (k, a) <- [ ("3", X.sendMessage (Toggle "ThreeCol"))
                            , ("x", X.sendMessage (MT.Toggle REFLECTX))
                            , ("y", X.sendMessage (MT.Toggle REFLECTY))
                            ]
                ])
            , ("<F5>", X.refresh)
            , ("C-j", X.sendMessage $ Go D)
            , ("C-k", X.sendMessage $ Go U)
            , ("C-h", X.sendMessage $ Go L)
            , ("C-l", X.sendMessage $ Go R)
            , ("j", BW.focusDown)
            , ("k", BW.focusUp)
            , ("m", X.windows W.focusMaster)
            , ("S-j", X.windows W.swapDown)
            , ("S-k", X.windows W.swapUp)
            , ("C-.", onGroup W.focusUp')
            , ("C-,", onGroup W.focusDown')
            , ("h", X.sendMessage X.Shrink)
            , ("l", X.sendMessage X.Expand)
            , ("<Return>", X.windows W.focusMaster)
            , ("S-<Return>", DwmP.dwmpromote)
            , ("t", X.withFocused $ X.windows . W.sink)
            , (",", X.sendMessage (X.IncMasterN 1))
            , (".", X.sendMessage (X.IncMasterN (-1)))

            -- keybindings for sublayouts
            , ("g", SUB.submap $ defaultSublMap conf)
            , ("<Left>", X.sendMessage $ pullGroup L)
            , ("<Right>", X.sendMessage $ pullGroup R)
            , ("<Up>", X.sendMessage $ pullGroup U)
            , ("<Down>", X.sendMessage $ pullGroup D)
            , ("C-m", X.withFocused (X.sendMessage . MergeAll))
            , ("C-u", X.withFocused (X.sendMessage . UnMerge))

            -- boring X.windows
            , ("b", BW.markBoring)
            , ("S-b", BW.clearBoring)

            -- minimized widnows
            , ("z", X.withFocused minimizeWindow)
            , ("S-z", X.sendMessage RestoreNextMinimizedWin)

              -- Toggle full screen
            , ("<F12>", X.sendMessage ToggleStruts >> X.refresh)

              -- Windows
            , ("S-c", X.kill)
            , ("C-S-c", WithAll.killAll)
            , ("x", X.spawn "xkill")

            -- Compositing
            , ("S-x", SUB.submap $ EZ.mkKeymap conf $ concat
                [ [(k, a), (modm ++ "-S-" ++ k, a)]
                | (k, a) <- [ ("r", X.spawn "systemctl --user restart compositing")
                            , ("s", X.spawn "systemctl --user stop compositing")]
                ])

              -- Workspaces
            , ("<Tab>", Local.toggleWS)
            , ("S-<Tab>", Local.toggleWSSwitch)
            , ("C-<Right>", moveTo Next nonEmptyWs)
            , ("]",         moveTo Next $ WSIs nonEmptyWsPred)
            , ("C-<Left>",  moveTo Prev $ WSIs nonEmptyWsPred)
            , ("[",         moveTo Prev $ WSIs nonEmptyWsPred)
            , ("-", SUB.submap $ EZ.mkKeymap conf
                [ (m ++ show k, _withNthWorkspace f i)
                | (k, i) <- (zip ([1..9] ++ [0]) [10..] :: [(Int, Int)])
                , (f, m) <- concat
                    [ [ -- switch to ith workspace
                        (W.greedyView, m)
                        -- shift focused to ith workspace
                      , (\ws -> W.greedyView ws . W.shift ws, m ++ "S-")
                      ]
                    | m <- ["", modm ++ "-"]
                    ]
                ])

            , ("n", promptedNewWorkspace False)
            , ("S-n", promptedNewWorkspace True)
            , ("S-<Backspace>", WithAll.killAll >> DW.removeWorkspace)
            , ("S-r", DW.renameWorkspace xpConfig)
            , ("c", TD.changeDir xpConfig)

            , ("r", swapScreens)

            , ("a", TS.currentTopicAction topicConfig)

            -- Grid Select workspace
            , ("i", goToSelected Local.gsConfig)
            , ("s", gsw)
            , ("S-s", gswShift)

              -- xmonad
            , ("q", SUB.submap $ EZ.mkKeymap conf $ concat
                [ [(k, a), (modm ++ "-" ++ k, a)]
                | (k, a) <- [ ("c", X.spawn "xmonad --recompile; xmonad --restart")
                            , ("r", X.spawn "xmonad --restart")
                            , ("u", X.spawn "undock")
                            , ("S-u", X.spawn "undock -s")
                            , ("e", X.spawn "monitor-hotplug")
                            , ("s", X.spawn "mate-session-save --shutdown-dialog")
                            , ("q", X.spawn "mate-session-save --logout")
                            , ("l", X.spawn "mate-screensaver-command --lock")
                            ]
                ])
            , ("C-q", X.spawn "mate-screensaver-command --lock")

            -- namedScratchpads
            , ("C-S-t", namedScratchpadAction namedScratchpads "htop")
            , ("C-S-a", namedScratchpadAction namedScratchpads "stardict")
            , ("C-S-n", namedScratchpadAction namedScratchpads "notes")
            , ("C-S-u", namedScratchpadAction namedScratchpads "charmap")
            , ("C-S-l", namedScratchpadAction namedScratchpads "alarm")
            , ("C-S-p", namedScratchpadAction namedScratchpads "volctl")

            -- misc
            , ("S-h", PSsh.sshPrompt xpConfig)
            --, ("<Print>", X.spawn "xfce4-screenshooter")
            , ("y", X.spawn "xfce4-popup-clipman")
            , ("<Print>", X.spawn "mate-screenshot")
            , ("C-<Print>", X.spawn "mate-screenshot -w")
            , ("S-<Print>", X.spawn "mate-screenshot -a")

            -- MPD
            -- mov current playing song in mpd to thrash
            , ("<Delete>", X.spawn "mpcrm")
            , ("<XF86Forward>",
                    X.io $ return . fromRight =<< MPD.withMPD MPD.next)
            , ("<XF86Back>",
                    X.io $ return . fromRight =<< MPD.withMPD MPD.previous)
            ]
         ++ [ (m ++ show i, _withNthWorkspace f ((i + 9) `mod` 10))
            | i <- [1..9] ++ [0]
            , (f, m) <- [ (W.greedyView, "") -- switch to ith workspace
                          -- shift focused to ith workspace
                        , (\ws -> W.greedyView ws . W.shift ws, "S-")
                        ]
            ]
         ++ [ (m ++ k, X.screenWorkspace sc >>= flip X.whenJust (X.windows . f))
            | (k, sc) <- zip ["w", "e"] [0..]
            , (f, m)  <- [(W.view, ""), (W.shift, "S-")]
            ]

    -- no prefix
    keys :: [(String, X.X())]
    keys = [
          ("<XF86Calculator>",
             namedScratchpadAction namedScratchpads "calculator")
        , ("<XF86Mail>", TS.switchTopic topicConfig "mail")
        , ("<XF86Terminal>", Local.spawnShell Nothing)
        , ("<XF86Explorer>", X.spawn "Terminal")
        , ("<XF86HomePage>", TS.switchTopic topicConfig "web")

        -- mpc
        , ("<XF86AudioPlay>",
                X.io $ return . fromRight =<< MPD.withMPD MPD.toggle)
        , ("<XF86AudioStop>",
                X.io $ return . fromRight =<< MPD.withMPD MPD.stop)
        , ("<XF86AudioNext>",
                X.io $ return . fromRight =<< MPD.withMPD MPD.next)
        , ("<XF86AudioPrev>",
                X.io $ return . fromRight =<< MPD.withMPD MPD.previous)

        -- volume
        , ("<XF86AudioMute>",        void toggleMute)
        , ("<XF86AudioRaiseVolume>", void (raiseVolume 4))
        , ("<XF86AudioLowerVolume>", void (lowerVolume 4))

        -- brightness
        , ("<XF86MonBrightnessUp>",     X.spawn "xbacklight +10")
        , ("<XF86MonBrightnessDown>",   X.spawn "xbacklight -10")
        , ("S-<XF86MonBrightnessUp>",   X.spawn "xbacklight +20")
        , ("S-<XF86MonBrightnessDown>", X.spawn "xbacklight -20")
        , ("C-<XF86MonBrightnessUp>",   X.spawn "xbacklight -set 100")
        , ("C-<XF86MonBrightnessDown>", X.spawn "xbacklight -set 0")
        ]

    _withNthWorkspace :: (String -> X.WindowSet -> X.WindowSet) -> Int -> X.X ()
    _withNthWorkspace job wnum = do
        sortfunc <- getSortByIndex
        ws <- X.gets ( map W.tag . sortfunc . namedScratchpadFilterOutWorkspace
                   . W.workspaces . X.windowset )
        case drop wnum ws of
             (w:_) -> X.windows $ job w
             [] -> return ()

nonEmptyWsPred :: X.X (X.WindowSpace -> Bool)
nonEmptyWsPred = do
    let ne = isJust . W.stack
    hs <- X.gets (map W.tag . W.hidden . X.windowset)
    let hi w = W.tag w `elem` hs
    return $ \w -> hi w && ne w && W.tag w /= "NSP"

-- cykle only NonEmpty, Hidden workspaces and not NSP workspaces
nonEmptyWs :: WSType
nonEmptyWs = WSIs nonEmptyWsPred
