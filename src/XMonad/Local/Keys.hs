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
import XMonad hiding (modMask, keys)
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Actions.DwmPromote as DwmP
import XMonad.Actions.GridSelect as GS
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
import qualified XMonad.Local.GridSelect as Local
import XMonad.Local.NamedScratchpad
import XMonad.Local.TopicSpace
import qualified XMonad.Local.Workspaces as Local

modMask :: KeyMask
modMask = mod4Mask
modm :: String
modm = "M4"

keyBindings :: XConfig l -> M.Map (KeyMask, KeySym) (X())
keyBindings conf = EZ.mkKeymap conf $ emacsKeys conf

emacsKeys :: XConfig l -> [(String, X())]
emacsKeys = \conf -> map prefix (keysMissingPrefix conf) ++ unprefixedKeys
  where
    prefix :: (String, a) -> (String, a)
    prefix (k, a) = (modm ++ "-" ++ k, a)

    keysMissingPrefix conf = concat $
        [ genericKeys conf
        , switchWorkspaceKeys
        , switchScreenKeys
        ]

-- need to be prefixed with modifier
genericKeys :: XConfig l -> [(String, X())]
genericKeys conf = [
           -- Applications
      (";", Local.spawnShell Nothing)
    , ("S-;", Local.spawnExplorer)
    , ("S-.", namedScratchpadAction namedScratchpads  "guake")
    , ("p", Shell.shellPrompt xpConfig)
    , ("S-p", Local.mateRun)

      -- Layouts
    , ("<Space>", sendMessage NextLayout)
    , ("C-<Space>", SUB.submap $ EZ.mkKeymap conf $ concat
        [ [(k, a), (modm ++ "-C-" ++ k, a)]
        | (k, a) <- [ ("3", sendMessage (Toggle "ThreeCol"))
                    , ("x", sendMessage (MT.Toggle REFLECTX))
                    , ("y", sendMessage (MT.Toggle REFLECTY))
                    ]
        ])
    , ("<F5>", refresh)
    , ("C-j", sendMessage $ Go D)
    , ("C-k", sendMessage $ Go U)
    , ("C-h", sendMessage $ Go L)
    , ("C-l", sendMessage $ Go R)
    , ("j", BW.focusDown)
    , ("k", BW.focusUp)
    , ("m", BW.focusMaster)
    , ("S-j", windows W.swapDown)
    , ("S-k", windows W.swapUp)
    , ("C-.", onGroup W.focusUp')
    , ("C-,", onGroup W.focusDown')
    , ("h", sendMessage Shrink)
    , ("l", sendMessage Expand)
    , ("<Return>", BW.focusMaster)
    , ("S-<Return>", DwmP.dwmpromote)
    , ("t", withFocused $ windows . W.sink)
    , (",", sendMessage (IncMasterN 1))
    , (".", sendMessage (IncMasterN (-1)))

    -- keybindings for sublayouts
    , ("g", SUB.submap $ defaultSublMap conf)
    , ("<Left>", sendMessage $ pullGroup L)
    , ("<Right>", sendMessage $ pullGroup R)
    , ("<Up>", sendMessage $ pullGroup U)
    , ("<Down>", sendMessage $ pullGroup D)
    , ("C-m", withFocused (sendMessage . MergeAll))
    , ("C-u", withFocused (sendMessage . UnMerge))

    -- minimized widnows
    , ("z", withFocused minimizeWindow)
    , ("S-z", sendMessage RestoreNextMinimizedWin)

      -- Toggle full screen
    , ("<F12>", sendMessage ToggleStruts >> refresh)

      -- Windows
    , ("S-c", kill)
    , ("C-S-c", WithAll.killAll)
    , ("x", spawn "xkill")

    -- Compositing
    , ("S-x", SUB.submap $ EZ.mkKeymap conf $ concat
        [ [(k, a), (modm ++ "-S-" ++ k, a)]
        | (k, a) <- [ ("r", spawn "systemctl --user restart compositing")
                    , ("s", spawn "systemctl --user stop compositing")]
        ])

      -- Workspaces
    , ("<Tab>", Local.toggleWS)
    , ("S-<Tab>", Local.toggleWSSwitch)
    , ("C-<Right>", moveTo Next nonEmptyWs)
    , ("]",         moveTo Next $ WSIs nonEmptyWsPred)
    , ("C-<Left>",  moveTo Prev $ WSIs nonEmptyWsPred)
    , ("[",         moveTo Prev $ WSIs nonEmptyWsPred)
    , ("-", SUB.submap $ EZ.mkKeymap conf
        [ (m ++ show k, withNthWorkspace f i)
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

    , ("n", Local.promptedNewWorkspace False)
    , ("S-n", Local.promptedNewWorkspace True)
    , ("S-<Backspace>", WithAll.killAll >> DW.removeWorkspace)
    , ("S-r", DW.renameWorkspace xpConfig)
    , ("c", TD.changeDir xpConfig)

    , ("r", Local.swapScreens)

    , ("a", TS.currentTopicAction topicConfig)

    -- Grid Select workspace
    , ("i", GS.goToSelected Local.gsConfig)
    , ("s", Local.gsw)
    , ("S-s", Local.gswShift)

      -- xmonad
    , ("q", SUB.submap $ EZ.mkKeymap conf $ concat
        [ [(k, a), (modm ++ "-" ++ k, a)]
        | (k, a) <- [ ("c", spawn "xmonad --recompile; xmonad --restart")
                    , ("r", spawn "xmonad --restart")
                    , ("u", spawn "undock")
                    , ("S-u", spawn "undock -s")
                    , ("e", spawn "monitor-hotplug")
                    , ("s", spawn "mate-session-save --shutdown-dialog")
                    , ("q", spawn "mate-session-save --logout")
                    , ("l", spawn "mate-screensaver-command --lock")
                    ]
        ])
    , ("C-q", spawn "mate-screensaver-command --lock")

    -- namedScratchpads
    , ("C-S-t", namedScratchpadAction namedScratchpads "htop")
    , ("C-S-a", namedScratchpadAction namedScratchpads "stardict")
    , ("C-S-n", namedScratchpadAction namedScratchpads "notes")
    , ("C-S-u", namedScratchpadAction namedScratchpads "charmap")
    , ("C-S-l", namedScratchpadAction namedScratchpads "alarm")
    , ("C-S-p", namedScratchpadAction namedScratchpads "volctl")

    -- misc
    , ("S-h", PSsh.sshPrompt xpConfig)
    --, ("<Print>", spawn "xfce4-screenshooter")
    , ("y", spawn "xfce4-popup-clipman")
    , ("<Print>", spawn "mate-screenshot")
    , ("C-<Print>", spawn "mate-screenshot -w")
    , ("S-<Print>", spawn "mate-screenshot -a")

    -- MPD
    -- mov current playing song in mpd to thrash
    , ("<Delete>", spawn "mpcrm")
    , ("<XF86Forward>",
            io $ return . fromRight =<< MPD.withMPD MPD.next)
    , ("<XF86Back>",
            io $ return . fromRight =<< MPD.withMPD MPD.previous)
    ]


switchWorkspaceKeys :: [(String, X())]
switchWorkspaceKeys =
    [ (m ++ show i, withNthWorkspace f ((i + 9) `mod` 10))
    | i <- [1..9] ++ [0]
    , (f, m) <- [ (W.greedyView, "") -- switch to ith workspace
                -- shift focused to ith workspace
                , (\ws -> W.greedyView ws . W.shift ws, "S-")
                ]
    ]

switchScreenKeys :: [(String, X())]
switchScreenKeys =
    [ (m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
    | (k, sc) <- zip ["w", "e"] [0..]
    , (f, m)  <- [(W.view, ""), (W.shift, "S-")]
    ]

-- no prefix
unprefixedKeys :: [(String, X())]
unprefixedKeys = [
      ("<XF86Calculator>",
         namedScratchpadAction namedScratchpads "calculator")
    , ("<XF86Mail>", TS.switchTopic topicConfig "mail")
    , ("<XF86Terminal>", Local.spawnShell Nothing)
    , ("<XF86Explorer>", spawn "Terminal")
    , ("<XF86HomePage>", TS.switchTopic topicConfig "web")

    -- mpc
    , ("<XF86AudioPlay>",
            io $ return . fromRight =<< MPD.withMPD MPD.toggle)
    , ("<XF86AudioStop>",
            io $ return . fromRight =<< MPD.withMPD MPD.stop)
    , ("<XF86AudioNext>",
            io $ return . fromRight =<< MPD.withMPD MPD.next)
    , ("<XF86AudioPrev>",
            io $ return . fromRight =<< MPD.withMPD MPD.previous)

    -- volume
    , ("<XF86AudioMute>",        void toggleMute)
    , ("<XF86AudioRaiseVolume>", void (raiseVolume 4))
    , ("<XF86AudioLowerVolume>", void (lowerVolume 4))

    -- brightness
    , ("<XF86MonBrightnessUp>",     spawn "xbacklight +10")
    , ("<XF86MonBrightnessDown>",   spawn "xbacklight -10")
    , ("S-<XF86MonBrightnessUp>",   spawn "xbacklight +20")
    , ("S-<XF86MonBrightnessDown>", spawn "xbacklight -20")
    , ("C-<XF86MonBrightnessUp>",   spawn "xbacklight -set 100")
    , ("C-<XF86MonBrightnessDown>", spawn "xbacklight -set 0")
    ]

withNthWorkspace :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthWorkspace job wnum = do
    sortfunc <- getSortByIndex
    ws <- gets ( map W.tag . sortfunc . namedScratchpadFilterOutWorkspace
               . W.workspaces . windowset )
    case drop wnum ws of
         (w:_) -> windows $ job w
         [] -> return ()

nonEmptyWsPred :: X (WindowSpace -> Bool)
nonEmptyWsPred = do
    let ne = isJust . W.stack
    hs <- gets (map W.tag . W.hidden . windowset)
    let hi w = W.tag w `elem` hs
    return $ \w -> hi w && ne w && W.tag w /= "NSP"

-- cykle only NonEmpty, Hidden workspaces and not NSP workspaces
nonEmptyWs :: WSType
nonEmptyWs = WSIs nonEmptyWsPred
