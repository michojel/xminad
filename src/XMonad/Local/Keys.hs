{-# LANGUAGE UnicodeSyntax #-}

module XMonad.Local.Keys (
      emacsKeys
    , keyBindings
    , modMask
    ) where

import           Control.Monad
import           Data.Either.Utils
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Network.MPD                      as MPD
import qualified Network.MPD.Commands.Extensions  as MPD
import           System.Posix.Signals             (sigCONT, sigSTOP)

import           XMonad                           hiding (keys, modMask)
import qualified XMonad.Actions.CopyWindow        as CW
import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.DwmPromote        as DwmP
import qualified XMonad.Actions.DynamicWorkspaces as DW
import           XMonad.Actions.GridSelect        as GS
import           XMonad.Actions.Minimize
import qualified XMonad.Actions.Submap            as SUB
import qualified XMonad.Actions.TopicSpace        as TS
import           XMonad.Actions.Volume
import qualified XMonad.Actions.WithAll           as WithAll
import           XMonad.Hooks.ManageDocks
import qualified XMonad.Layout.BoringWindows      as BW
import qualified XMonad.Layout.MultiToggle        as MT
import           XMonad.Layout.Reflect
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.ToggleLayouts
import qualified XMonad.Layout.TopicDir           as TD
import           XMonad.Layout.WindowNavigation
import qualified XMonad.Prompt.Shell              as Shell
import qualified XMonad.Prompt.Ssh                as PSsh
import qualified XMonad.StackSet                  as W
import qualified XMonad.Util.EZConfig             as EZ
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare     (getSortByIndex)

-- local modules **************************************************************
import qualified XMonad.Local.Actions             as Local
import           XMonad.Local.Config
import qualified XMonad.Local.GridSelect          as Local
import qualified XMonad.Local.Music               as Local
import           XMonad.Local.NamedScratchpad
import           XMonad.Local.Operations          as Op
import           XMonad.Local.TopicSpace
import qualified XMonad.Local.Workspaces          as Local

modMask ∷ KeyMask
modMask = mod4Mask
modm ∷ String
modm = "M4"

keyBindings ∷ XConfig l → M.Map (KeyMask, KeySym) (X())
keyBindings conf = EZ.mkKeymap conf $ emacsKeys conf

emacsKeys ∷ XConfig l → [(String, X())]
emacsKeys = \conf -> map prefix (keysMissingPrefix conf) ++ unprefixedKeys
  where
    prefix ∷ (String, a) → (String, a)
    prefix (k, a) = (modm ++ "-" ++ k, a)

    keysMissingPrefix conf = concat
        [ genericKeys conf
        , switchWorkspaceKeys
        , wspActionKeys conf
        , switchScreenKeys
        ]

-- need to be prefixed with modifier
genericKeys ∷ XConfig l → [(String, X())]
genericKeys conf = [
      -- Applications
      (";", Local.spawnShell)
    , ("S-;", Local.spawnExplorer)
    , ("S-.", namedScratchpadAction namedScratchpads  "guake")
    , ("p", Shell.shellPrompt xpConfig)
    , ("S-p", safeSpawnProg "lxqt-runner")

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
    , ("S-z", withLastMinimized maximizeWindowAndFocus)

      -- Toggle full screen
    , ("<F12>", sendMessage ToggleStruts >> refresh)

      -- Windows
    -- copy window
    , ("c", SUB.submap $ EZ.mkKeymap conf $ extendKeyListForMod $ concat $
        -- to particular workspace by index
        [ [ (show i,         withNthWorkspace CW.copy         ((i + 9) `mod` 10))
          , ("S-" ++ show i, withNthWorkspace copyWinAndFocus ((i + 9) `mod` 10))]
        | i <- [1..9] ++ [0]
        ] ++
        -- to a screen
        -- NOTE: the copied window will not be visible on a non-focused screen
        [ [ (k,         screenWorkspace i >>= flip whenJust (windows . CW.copy))
          , ("S-" ++ k, screenWorkspace i >>= flip whenJust (windows . (\ws -> W.view ws . CW.copy ws))) ]
        | (k, i) <- zip ["w", "e", "r"] [0..]
        ] ++ -- to all workspaces
        [ [ ("a", windows CW.copyToAll)
            -- to a new workspace
            -- TODO: make this work
          -- , ("n", Local.getpromptedNewWorkspace False)
            -- to a workspace selected with grid select
          , ("s",   Local.gswinDo CW.copy)
          , ("S-s", Local.gswinDo copyWinAndFocus)
          ]
        ])
    -- kill window
    , ("x", SUB.submap $ EZ.mkKeymap conf $ concat
        [ [(k, a), (modm ++ "-" ++ k, a)]
        | (k, a) <- [ ("s",        Local.signalCurrentWindow sigSTOP)
                    , ("c",        Local.signalCurrentWindow sigCONT)
                    , ("x",        safeSpawnProg "xkill")
                    , ("k",        kill)        -- kill all the copies of the window
                    , ("<Return>", CW.kill1)    -- kill just one copy of the window
                    , ("1",        CW.kill1)    -- kill just one copy of the window
                    , ("o",        CW.killAllOtherCopies)   
                    , ("a",        WithAll.killAll)   
                    ]
        ])

    -- Compositing
    , ("S-x", SUB.submap $ EZ.mkKeymap conf $ concat
        [ [(k, a), (modm ++ "-S-" ++ k, a)]
        | (k, a) <- [ ("r", safeSpawn "systemctl" ["--user", "restart", "compositing"])
                    , ("s", safeSpawn "systemctl" ["--user", "stop", "compositing"])]
        ])

      -- Workspaces
    , ("<Tab>",     Local.toggleWS)
    , ("S-<Tab>",   Local.toggleWSSwitch)
    , ("C-<Right>", moveTo Next nonEmptyWs)
    , ("]",         moveTo Next $ WSIs nonEmptyWsPred)
    , ("C-<Left>",  moveTo Prev $ WSIs nonEmptyWsPred)
    , ("[",         moveTo Prev $ WSIs nonEmptyWsPred)
    , ("-",         switchWorkspaceSubmap conf 10)
    , ("S--",       switchWorkspaceSubmap conf 20)

    , ("n", Local.promptedNewWorkspace False)
    , ("S-n", Local.promptedNewWorkspace True)
    , ("S-<Backspace>", WithAll.killAll >> DW.removeWorkspace)
    , ("S-r", DW.renameWorkspace xpConfig)
    , ("S-c", TD.changeDir xpConfig)

    , ("S-r", Local.swapScreens)

    , ("a", TS.currentTopicAction topicConfig)

    -- Grid Select workspace
    , ("i", GS.goToSelected Local.gsConfig)
    , ("s", Local.gsw)
    , ("S-s", Local.gswinShift)

      -- xmonad
    , ("q", SUB.submap $ EZ.mkKeymap conf $ concat
        [ [(k, a), (modm ++ "-" ++ k, a)]
        | (k, a) <- [ ("r", safeSpawn     "pkill" ["-x", "lxqt-panel"]
                         >> safeSpawn     "pkill" ["-x", "xmobar"]
                         >> Op.restart)
                    , ("u", safeSpawnProg "undock")
                    , ("S-u", safeSpawn   "undock" ["-s"])
                    , ("e", safeSpawnProg "monitor-hotplug")
                    , ("s", safeSpawnProg "lxqt-leave")
                    , ("q", safeSpawn     "lxqt-leave" ["--logout"])
                    , ("l", safeSpawn     "xautolock" ["-locknow"])
                    ]
        ])
    , ("C-q", safeSpawn "xautolock" ["-locknow"])

    -- namedScratchpads
    , ("C-S-h", namedScratchpadAction namedScratchpads "htop")
    , ("C-S-m", namedScratchpadAction namedScratchpads "man-browser")
    , ("C-S-a", namedScratchpadAction namedScratchpads "dictionary")
    , ("C-S-n", namedScratchpadAction namedScratchpads "notes")
    , ("C-S-u", namedScratchpadAction namedScratchpads "charmap")
    , ("C-S-l", namedScratchpadAction namedScratchpads "alarm")
    , ("C-S-p", namedScratchpadAction namedScratchpads "volctl")
    , ("C-S-t", namedScratchpadAction namedScratchpads "tabsoutliner")

    -- misc
    , ("S-h", PSsh.sshPrompt xpConfig)
    , ("v", Local.pastePlainTextFromClipboard)
    , ("d", Local.getAndPasteDigraph)
    --, ("<Print>", safeSpawnProg "xfce4-screenshooter")
    , ("y", SUB.submap $ EZ.mkKeymap conf $ concat
        [ [(k, a), (modm ++ "-" ++ k, a)]
        | (k, a) <- [ ("n",       io $ fmap fromRight (MPD.withMPD MPD.next))
                    , ("p",       io $ fmap fromRight (MPD.withMPD MPD.previous))
                    , ("S-.",     io $ fmap fromRight (MPD.withMPD MPD.next))
                    , ("S-,",     io $ fmap fromRight (MPD.withMPD MPD.previous))
                    , ("y",       io $ fmap fromRight (MPD.withMPD (MPD.play Nothing)))
                    , ("s",       io $ fmap fromRight (MPD.withMPD MPD.stop))
                    , ("r",       io $ fmap fromRight (MPD.withMPD Local.toggleRepeat))
                    , ("*",       io $ fmap fromRight (MPD.withMPD Local.toggleRandom))
                    , ("S-8",     io $ fmap fromRight (MPD.withMPD Local.toggleRandom))
                    , ("<Space>", io $ fmap fromRight (MPD.withMPD MPD.toggle))
                    ]
        ])
    , ("<Print>",   unsafeSpawn "scrot -u ~/Pictures/%Y-%m-%d-%T-window-screenshot.png")
    , ("C-<Print>", unsafeSpawn "scrot -s ~/Pictures/%Y-%m-%d-%T-screenshot.png")
    , ("S-<Print>", unsafeSpawn "scrot ~/Pictures/%Y-%m-%d-%T-root-screenshot.png")

    -- MPD
    -- mov current playing song in mpd to thrash
    , ("<Delete>",      safeSpawnProg "mpcrm")
    , ("<XF86Forward>", io $ fmap fromRight (MPD.withMPD MPD.next))
    , ("<XF86Back>",    io $ fmap fromRight (MPD.withMPD MPD.previous))
    ]

extendKeyListForMod :: [(String, b)] -> [(String, b)]
extendKeyListForMod = foldr modcompose []
    where
        -- for any given key and action, allow both modm-prefixed and unprefixed key combos
        modcompose (k, a) t = (modm ++ "-" ++ k, a):(k, a):t

switchWorkspaceSubmap ∷ XConfig l → Int → X()
switchWorkspaceSubmap conf base = SUB.submap $ EZ.mkKeymap conf
        [ (m ++ show k, withNthWorkspace f i)
        | (k, i) <- zip ([1..9] ++ [0]) [base..] ∷ [(Int, Int)]
        , (f, m) <- concat
            [ [ -- switch to (base + i)th workspace
                (W.greedyView, m)
                -- shift focused to (base + i)th workspace
              , (shiftWinAndFocus, m ++ "S-")
              ]
            | m <- ["", modm ++ "-"]
            ]
        ]

switchWorkspaceKeys ∷  [(String, X())]
switchWorkspaceKeys =
    [ (m ++ show i, withNthWorkspace f ((i + 9) `mod` 10))
    | i <- [1..9] ++ [0]
    , (f, m) <- [ (W.greedyView, "") -- switch to ith workspace
                -- shift focused to ith workspace
                , (shiftWinAndFocus, "S-")
                ]
    ]


wspActionKeys ∷ XConfig l -> [(String, X())]
wspActionKeys conf =
    [ ("C-" ++ show i, wspActionSubmap conf ((i + 9) `mod` 10))
    | i <- [1..9] ++ [0]
    ]

wspActionSubmap ∷ XConfig m -> Int -> X()
wspActionSubmap conf n = SUB.submap $ EZ.mkKeymap conf $ concat
        [ [(k, a), ("C-" ++ modm ++ "-" ++ k, a), (modm ++ "-" ++ k, a)]
        | (k, a) <- [ ("c", wspDo   CW.copy)
                    , ("a", windows CW.copyToAll)
                    , ("s", wspDo   W.shift)
                    , ("f", wspDo   shiftWinAndFocus)
                    ]
        ]
    where
        wspDo :: (String → WindowSet → WindowSet) → X ()
        wspDo f = withNthWorkspace f n

shiftWinAndFocus :: (Ord a, Eq s, Eq i) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
shiftWinAndFocus ws = W.greedyView ws . W.shift ws

copyWinAndFocus :: (Ord a, Eq s, Eq i) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyWinAndFocus ws = W.greedyView ws . CW.copy ws

switchScreenKeys ∷ [(String, X())]
switchScreenKeys =
    [ (m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
    | (k, sc) <- zip ["w", "e", "r"] [0..]
    , (f, m)  <- [(W.view, ""), (W.shift, "S-")]
    ]

-- no prefix
unprefixedKeys ∷ [(String, X())]
unprefixedKeys = [
      ("<XF86Calculator>",
         namedScratchpadAction namedScratchpads "calculator")
    , ("<XF86Mail>", TS.switchTopic topicConfig "mail")
    , ("<XF86Terminal>", Local.spawnShell)
    , ("<XF86Explorer>", Local.spawnExplorer)
    , ("<XF86HomePage>", TS.switchTopic topicConfig "web")

    -- mpc
    , ("<XF86AudioPlay>", io $ fmap fromRight (MPD.withMPD MPD.toggle))
    , ("<XF86AudioStop>", io $ fmap fromRight (MPD.withMPD MPD.stop))
    , ("<XF86AudioNext>", io $ fmap fromRight (MPD.withMPD MPD.next))
    , ("<XF86AudioPrev>", io $ fmap fromRight (MPD.withMPD MPD.previous))

    -- volume
    , ("<XF86AudioMute>",        void toggleMute)
    , ("<XF86AudioRaiseVolume>", void (raiseVolume 4))
    , ("<XF86AudioLowerVolume>", void (lowerVolume 4))

    -- brightness
    , ("<XF86MonBrightnessUp>",     safeSpawn "xbacklight" ["+10"])
    , ("<XF86MonBrightnessDown>",   safeSpawn "xbacklight" ["-10"])
    , ("S-<XF86MonBrightnessUp>",   safeSpawn "xbacklight" ["+20"])
    , ("S-<XF86MonBrightnessDown>", safeSpawn "xbacklight" ["-20"])
    , ("C-<XF86MonBrightnessUp>",   safeSpawn "xbacklight" ["-set", "100"])
    , ("C-<XF86MonBrightnessDown>", safeSpawn "xbacklight" ["-set", "0"])

    , ("C-S-<F9>", safeSpawn "xset" ["dpms", "force", "off", "&&", "sleep", "0.5", "&&", "xset", "dpms", "force", "on"])
    , ("<XF86Tools>", safeSpawn "xset" ["dpms", "force", "off", "&&", "sleep", "0.5", "&&", "xset", "dpms", "force", "on"])
    ]

withNthWorkspace ∷ (String → WindowSet → WindowSet) → Int → X ()
withNthWorkspace job wnum = do
    sortfunc <- getSortByIndex
    ws <- gets ( map W.tag . sortfunc . namedScratchpadFilterOutWorkspace
               . W.workspaces . windowset )
    case drop wnum ws of
         (w:_) -> windows $ job w
         []    -> return ()

nonEmptyWsPred ∷ X (WindowSpace → Bool)
nonEmptyWsPred = do
    let ne = isJust . W.stack
    hs <- gets (map W.tag . W.hidden . windowset)
    let hi w = W.tag w `elem` hs
    return $ \w -> hi w && ne w && W.tag w /= "NSP"

-- cykle only NonEmpty, Hidden workspaces and not NSP workspaces
nonEmptyWs ∷ WSType
nonEmptyWs = WSIs nonEmptyWsPred
