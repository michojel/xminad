{-# LANGUAGE NoMonomorphismRestriction, DoAndIfThenElse #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

import qualified Codec.Binary.UTF8.String as UTF8
--import Control.Exception
import Control.Monad
import Data.Either.Utils
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String.Utils (startswith)
import qualified Data.Set as S
import qualified DBus as D
import qualified DBus.Client as D
import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPD
import Text.Regex
import Text.Regex.Posix
--import System.Exit
--import System.IO

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect as GS
import XMonad.Actions.Volume
--import XMonad.Actions.UpdateFocus
import qualified XMonad.Actions.DwmPromote as DwmP
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Actions.FlexibleResize as FlexR
import qualified XMonad.Actions.Submap as SUB
import qualified XMonad.Actions.TopicSpace as TS
import qualified XMonad.Actions.UpdatePointer as UP
import qualified XMonad.Actions.WithAll as WithAll
import XMonad.Config.Desktop
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Minimize
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.Reflect
import XMonad.Layout.SubLayouts
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import qualified XMonad.Prompt as P
import qualified XMonad.Prompt.Shell as Shell
import qualified XMonad.Prompt.Input as PI
import qualified XMonad.Prompt.Ssh as PSsh
import qualified XMonad.StackSet as W
import qualified XMonad.Util.EZConfig as EZ
import XMonad.Util.NamedScratchpad as NS
--import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByIndex)

-- local modules **************************************************************
import qualified XMonad.Layout.TopicDir as TD
import qualified XMonad.Local.Actions as Local
import qualified XMonad.Local.Layout as Local
import qualified XMonad.Local.TopicSpace as Local

myModMask :: KeyMask
myModMask = mod4Mask
modm :: String
modm = "M4"
myTerminal :: String
myTerminal = "mate-terminal"

doNotFadeOutWindows :: Query Bool
doNotFadeOutWindows =
    className =? "xine" <||>
    className =? "MPlayer" <||>
    className =? "Smplayer" <||>
    className =? "Vlc" <||>
    className =? "Firefox" <||>
    className =? "Opera" <||>
    className =? "Shiretoko" <||>
    className =? "VirtualBox" <||>
    className =? "Namoroka" <||>
    className =? "Navigator" <||>
    className =? "Chromium" <||>
    className =? "Google-chrome" <||>
    className =? "Civ5XP" <||>
    className =? "BaldursGate" <||>
    title     =? "VLC (XVideo output)"

myNamedScratchpads :: [NamedScratchpad]
myNamedScratchpads =
        [ NS "htop" (myTerminal ++ " -t htop -e htop") (title =? "htop")
            cTopFloat
        , NS "stardict" "stardict" (className =? "Stardict") cFloating
        , NS "notes" "gvim --role notes ~/notes.txt" (role =? "notes")
            cFloating
        , NS "charmap" "charmap" (className =? "Gucharmap") cFloating
        , NS "alarm" "alarm-clock-applet"
             (className =? "Alarm-clock-applet") cFloating
        , NS "calculator" (myTerminal ++ " -e python --title PCalculator")
                           (title =? "PCalculator") cFloating
        , NS "volctl" "mate-volume-control" (className =? "Mate-volume-control") cFloating
        , NS "guake" (myTerminal ++ " --window-with-profile=Guake-normal --tab-with-profile=Guake-root")
             (className =? "Mate-terminal" <&&> (startsWith title "Guake")) cBottomFloat 
        ]
    where
        role = stringProperty "WM_WINDOW_ROLE"
        cFloating = customFloating $ W.RationalRect (1/3) (1/9) (1/3) (1/3)
        cTopFloat = customFloating $ W.RationalRect (1/5) (1/32) (3/5) (1/2)
        cBottomFloat = customFloating $ W.RationalRect (1/5) (5/8) (3/5) (3/8)
        startsWith q x = fmap (startswith x) q

myXPConfig :: P.XPConfig
myXPConfig = P.defaultXPConfig
    { P.fgColor = "#dfdfdf"
    , P.bgColor = "#3c3c3c"
    , P.fgHLight = "#ffffff"
    , P.bgHLight = "#3c3c3c"
    , P.font    = "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*"
    , P.height  = 24
    }

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["dashboard", "devel"]

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = GS.defaultGSConfig
    { gs_cellheight = 40
    , gs_cellwidth = 100
    , gs_navigate = navigation'
    }
  where
    navigation' :: TwoD a (Maybe a)
    navigation' = GS.makeXEventhandler
                $ GS.shadowWithKeymap navKeyMap navHandler

    navKeyMap = M.fromList (allowModifs modifs
          [ ((0,xK_Escape)     , cancel)
          , ((0,xK_Return)     , select)
          , ((0,xK_slash)      , substringSearch navigation')
          , ((0,xK_question)   , substringSearch navigation')
          , ((0,xK_Left)       , move (-1,0) >> navigation')
          , ((0,xK_h)          , move (-1,0) >> navigation')
          , ((0,xK_H)          , move (-1,0) >> navigation')
          , ((0,xK_Right)      , move (1,0) >> navigation')
          , ((0,xK_l)          , move (1,0) >> navigation')
          , ((0,xK_L)          , move (1,0) >> navigation')
          , ((0,xK_Down)       , move (0,1) >> navigation')
          , ((0,xK_j)          , move (0,1) >> navigation')
          , ((0,xK_J)          , move (0,1) >> navigation')
          , ((0,xK_Up)         , move (0,-1) >> navigation')
          , ((0,xK_k)          , move (0,-1) >> navigation')
          , ((0,xK_K)          , move (0,-1) >> navigation')
          , ((0,xK_n)          , moveNext >> navigation')
          , ((0,xK_N)          , moveNext >> navigation')
          , ((0,xK_p)          , movePrev >> navigation')
          , ((0,xK_P)          , movePrev >> navigation')
          ]
        ++ allowModifs (drop 1 modifs)
          [ ((0,xK_Tab)         , moveNext >> navigation')
          , ((shiftMask,xK_Tab) , moveNext >> navigation')
          ]
        )
    modifs :: [KeyMask]
    modifs = [ shiftMask, lockMask, mod1Mask, mod2Mask
             , mod3Mask, mod4Mask, mod5Mask ]

    allowModifs :: [ KeyMask ] -> [((KeyMask, a), b)] -> [((KeyMask, a), b)]
    allowModifs mods keymap = [ ((m .|. o, k), a)
            | m <- map (foldl (.|.) 0) $ subsequences mods
            , ((o, k), a) <- keymap ]

    -- The navigation handler ignores unknown key symbols
    navHandler = const navigation'

nonEmptyWsPred :: X (WindowSpace -> Bool)
nonEmptyWsPred = do
    let ne = isJust . W.stack
    hs <- gets (map W.tag . W.hidden . windowset)
    let hi w = W.tag w `elem` hs
    return $ \w -> hi w && ne w && W.tag w /= "NSP"

swapScreens :: X ()
swapScreens = do
    screen <- gets (listToMaybe . W.visible . windowset)
    whenJust screen $ windows . W.greedyView . W.tag . W.workspace

{- cykle only NonEmpty, Hidden workspaces and not NSP workspaces -}
nonEmptyWs :: WSType
nonEmptyWs = WSIs nonEmptyWsPred

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X())
myKeys conf = EZ.mkKeymap conf $ emacsKeys conf

emacsKeys :: XConfig l -> [(String, X())]
emacsKeys = \conf -> map prefix (emacsKeys' conf) ++ keys'
  where
    prefix :: (String, a) -> (String, a)
    prefix (k, a) = (modm ++ "-" ++ k, a)

    emacsKeys' :: XConfig l -> [(String, X())]
    emacsKeys' conf = [
               -- Applications
              (";", Local.spawnShell Nothing)
            , ("S-;", Local.spawnExplorer)
            , ("S-.", namedScratchpadAction myNamedScratchpads  "guake")
            , ("p", Shell.shellPrompt myXPConfig)
            , ("S-p", mateRun)

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
            , ("m", windows W.focusMaster)
            , ("S-j", windows W.swapDown)
            , ("S-k", windows W.swapUp)
            , ("C-.", onGroup W.focusUp')
            , ("C-,", onGroup W.focusDown')
            , ("h", sendMessage Shrink)
            , ("l", sendMessage Expand)
            , ("<Return>", windows W.focusMaster)
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

            -- boring windows
            , ("b", BW.markBoring)
            , ("S-b", BW.clearBoring)

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
            , ("<Tab>", myToggleWS)
            , ("S-<Tab>", toggleWSSwitch)
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
            , ("S-r", DW.renameWorkspace myXPConfig)
            , ("c", TD.changeDir myXPConfig)

            , ("r", swapScreens)

            , ("a", TS.currentTopicAction Local.topicConfig)

            -- Grid Select workspace
            , ("i", goToSelected myGSConfig)
            , ("s", gsw)
            , ("S-s", gswShift)

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

            -- myNamedScratchpads
            , ("C-S-t", namedScratchpadAction myNamedScratchpads "htop")
            , ("C-S-a", namedScratchpadAction myNamedScratchpads "stardict")
            , ("C-S-n", namedScratchpadAction myNamedScratchpads "notes")
            , ("C-S-u", namedScratchpadAction myNamedScratchpads "charmap")
            , ("C-S-l", namedScratchpadAction myNamedScratchpads "alarm")
            , ("C-S-p", namedScratchpadAction myNamedScratchpads "volctl")

            -- misc
            , ("S-h", PSsh.sshPrompt myXPConfig)
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
         ++ [ (m ++ show i, _withNthWorkspace f ((i + 9) `mod` 10))
            | i <- [1..9] ++ [0]
            , (f, m) <- [ (W.greedyView, "") -- switch to ith workspace
                          -- shift focused to ith workspace
                        , (\ws -> W.greedyView ws . W.shift ws, "S-")
                        ]
            ]
         ++ [ (m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
            | (k, sc) <- zip ["w", "e"] [0..]
            , (f, m)  <- [(W.view, ""), (W.shift, "S-")]
            ]

    -- no prefix
    keys' :: [(String, X())]
    keys' = [
          ("<XF86Calculator>",
             namedScratchpadAction myNamedScratchpads "calculator")
        , ("<XF86Mail>", TS.switchTopic Local.topicConfig "mail")
        , ("<XF86Terminal>", Local.spawnShell Nothing)
        , ("<XF86Explorer>", spawn "Terminal")
        , ("<XF86HomePage>", TS.switchTopic Local.topicConfig "web")

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

    _withNthWorkspace :: (String -> WindowSet -> WindowSet) -> Int -> X ()
    _withNthWorkspace job wnum = do
        sortfunc <- getSortByIndex
        ws <- gets ( map W.tag . sortfunc . namedScratchpadFilterOutWorkspace
                   . W.workspaces . windowset )
        case drop wnum ws of
             (w:_) -> windows $ job w
             [] -> return ()


myToggleWS :: X()
myToggleWS = do
    hs' <- cleanHiddens ["NSP"]
    unless (null hs') (windows . W.greedyView . W.tag $ head hs')
  where
    cleanHiddens :: [WorkspaceId] -> X [WindowSpace]
    cleanHiddens skips = gets $ flip skipTags skips . W.hidden . windowset

toggleWSSwitch :: X()
toggleWSSwitch = do
    hs' <- cleanHiddens []
    unless (null hs') (windows . (\ws -> W.greedyView ws . W.shift ws) . W.tag $ head hs')
  where
    cleanHiddens :: [WorkspaceId] -> X [WindowSpace]
    cleanHiddens skips =  gets $ flip skipTags skips . W.hidden . windowset

promptedNewWorkspace :: Bool -> X()
promptedNewWorkspace shiftFocused = PI.inputPrompt myXPConfig "New Workspace"
                                  PI.?+ action shiftFocused
  where
    action :: Bool -> String -> X()
    action True = newWorkspaceDir shiftto
    action _    = newWorkspaceDir goto

-- creates the workspace if needed
goto :: TS.Topic -> X()
goto t = newWorkspace t >> TS.switchTopic Local.topicConfig t
shiftto :: TS.Topic -> X()
shiftto t = newWorkspace t >> windows (W.greedyView t . W.shift t)

newWorkspace :: WorkspaceId -> X()
newWorkspace w = do
    exists <- widExists w
    unless exists $ DW.addHiddenWorkspace w

newWorkspaceDir :: (TS.Topic -> X()) -> WorkspaceId -> X()
newWorkspaceDir gotofunc w = do
    exists <- widExists w
    if not exists then do
        DW.addHiddenWorkspace w
        gotofunc w
        unless (w `S.member` doNotAskForDir) $ TD.changeDir myXPConfig
    else
        gotofunc w
  where
    doNotAskForDir :: S.Set WorkspaceId
    doNotAskForDir = S.fromList $
         ["mail", "chat", "virt", "vbox", "web"] ++ M.keys Local.topicDirs

widExists :: WorkspaceId -> X Bool
widExists wid = do
    xs <- get
    return $ widExists' wid (windowset xs)
  where
    widExists' :: WorkspaceId -> W.StackSet WorkspaceId l a s sd -> Bool
    widExists' w ws = w `elem` map W.tag (W.workspaces ws)

switchTopic' :: (WorkspaceId -> WindowSet -> WindowSet)
                -> TS.Topic -> X ()
switchTopic' viewMethod topic = do
   windows $ viewMethod topic
   wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
   when (null wins) $ TS.topicAction Local.topicConfig topic

gsw :: X()
gsw = gsw' W.greedyView
  where
    gsw' :: (WorkspaceId -> WindowSet -> WindowSet) -> X ()
    gsw' viewFunc = withWindowSet $ \ws -> do
        let wss = map W.tag $ fHidden ws ++ map W.workspace (W.current ws : W.visible ws)
        gridselect myGSConfig (zip wss wss) >>= flip whenJust (switchTopic' viewFunc)
    fHidden = filter ((/=) "NSP" . W.tag) . W.hidden

gswShift :: X()
gswShift = gridselectWorkspace myGSConfig (\ws -> W.greedyView ws . W.shift ws)

mateRun :: X ()
mateRun = withDisplay $ \dpy -> do
    rw <- asks theRoot
    mate_panel <- getAtom "_MATE_PANEL_ACTION"
    panel_run   <- getAtom "_MATE_PANEL_ACTION_RUN_DIALOG"

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw mate_panel 32 panel_run 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False

myBaseConfig = desktopConfig
    { XMonad.modMask = myModMask
    }

-- Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig Layout
                -> M.Map (ButtonMask, Button) (Window -> X())
myMouseBindings (XConfig {XMonad.modMask = mm}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((mm, button1), \w -> focus w >> mouseMoveWindow w
                                    >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((mm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((mm, button3), \w -> focus w >> FlexR.mouseResizeWindow w)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((mm, button4), const $ windows W.swapDown)
    , ((mm, button5), const $ windows W.swapUp)
    ]


{- note: earlier hooks override later ones -}
myManageHook :: ManageHook
myManageHook = composeOne (concat
        [ --[manageHook myBaseConfig]
          [checkDock -?> doIgnore]
        , [className =? c -?> doIgnore | c <- myCIgnores]
        , [isFullscreen -?> doMaster <+> doFullFloat]
        , [transience]
        , [isDialog -?> doMaster <+> doCenterFloat]

        , [fmap ("Preferences" `isInfixOf`) title -?> doMaster <+> doCenterFloat]
        , [(className =? "Gimp" <&&> c) -?> h | (c, h) <- gimpManage]
        , [(    className =? "Qjackctl"
           <&&> fmap ("JACK Audio Connection Kit" `isPrefixOf`) title)
            -?> doMaster <+> doFloat]
        , [(className =? "Dia" <&&> role =? "toolbox_window")
            -?> doMaster <+> doFloat]

        , [ className =? c <&&> title =? t -?> doMaster <+> doFloat
          | (c, t) <- myCTFloats ]
        , [className =? c -?> doMaster <+> doCenterFloat | c <- myCCenterFloats]
        , [className =? "Virt-manager" <&&> title =? "New VM"
          -?> doMaster <+> doCenterFloat]
        , [className =? c -?> doMaster <+> doFloat | c <- myCFloats ]
        , [title =? t -?> doMaster <+> doFloat | t <- myTFloats ]
        , [ className =? "BaldursGate" -?> doMyShift "BG" <+> doMaster]
        , [query c -?> hook c | c <- myNamedScratchpads]])
        --, [className =? "dzen" -?> transparency 0.4]])
    <+>
    composeOne (concat
        [ [className =? "Dia"          -?> doMyShift "dia"]
        , [className =? c              -?> doMyShift "chat" | c <- myChatClients ]
        , [(className =? "Google-chrome" <&&> title =? "Hangouts") -?> doMyShift "chat"]
        , [className =? c              -?> doMyShift "web"  | c <- myWebBrowsers ]
        , [title =? "ncmpcpp"          -?> doMyShift "music" ]
        , [className =? c              -?> doMyShift "music" | c <- myMusicPlayers ]
        , [className =? c              -?> doMyShift "video" | c <- myVideoPlayers ]
        , [className =? "Evince"       -?> doMyShift "pdf" ]
        , [className =? "Atril"        -?> doMyShift "pdf" ]
        , [className =? "Thunderbird"  -?> doMyShift "mail"]
        , [className =? "Gimp"         -?> doMyShift "gimp"]
        , [className =? "Virt-manager" -?> doMyShift "virt"]
        , [className =? "VirtualBox"   -?> doMyShift "vbox"]
        , [className =? "Deluge"       -?> doMyShift "p2p"]
        , [className =? "Calibre"      -?> doMyShift "ebook"]
        , [className =? "Squeak"       -?> doMyShift "squeak"]
        , [className =? "Civ5XP"       -?> doMyShift "ciV"]
        , [className =? "Googleearth-bin" -?> doMyShift "earth"]
        -- see http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Hooks-XPropManage.html#xPropManageHook
        ])
  where
    doMaster = doF W.shiftMaster
    doMyShift :: WorkspaceId -> ManageHook
    doMyShift wsp = do
        liftX (newWorkspace wsp)
        doF $ W.greedyView wsp . W.shift wsp
    myTFloats = [ "VLC (XVideo output)"
              , "DownThemAll! - Make Your Selection"
              , "Add Downloads"
              , "Add URL(s)"
              , "Run Application"
              ]
    myCIgnores = [ "Xfce4-notifyd"
                 ]
    myCFloats = [ -- "Gnome-panel"
                  "Close session"
                , "MPlayer"
                , "Wine"
                , "Galculator"
                ]
    myCTFloats = [ ("Skype", "Information")
                 , ("Firefox", "Certificate Manager")
                 , ("processing-app-Base", "Preferences")
                 , ("Thunar", "File Operation Progress")
                 ]
    myCCenterFloats = [ -- "Gnome-tweak-tool"
                        "Xfce4-notes"
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
    myChatClients  = ["Pidgin", "Xchat", "Skype", "Empathy", "Hexchat"]
    myWebBrowsers  = [ -- "Firefox"
          "Chromium-browser"
        , "Google-chrome"
        ]
    myMusicPlayers = ["ncmpcpp", "Sonata", "Rhythmbox", "Gmpc"]
    myVideoPlayers = ["MPlayer", "Vlc", "Smplayer"]
    gimpManage = [ ( role =? "gimp-toolbox" <||> role =? "gimp-image-window"
                   , ask >>= doF . W.sink)
                 , (role =? "gimp-image-merge-layers", doCenterFloat)
                 , (title =? "Scale Image", doCenterFloat)
                 , (title =? "Export File", doCenterFloat)
                 , (fmap ("Save as" `isPrefixOf`) title, doCenterFloat)
                 ]

    role = stringProperty "WM_WINDOW_ROLE"

myFadeHook :: FadeHook
myFadeHook = composeAll [ opaque
                        , isUnfocused         --> transparency 0.2
                        , doNotFadeOutWindows --> opaque
                        ]

--myLogHook :: Handle -> X ()
myLogHook dbus = do
    sorted <- getSortByIndex
    ws <- gets ( map W.tag . sorted . namedScratchpadFilterOutWorkspace
               . W.workspaces . windowset
               )
    DL.dynamicLogWithPP (myPP $ M.fromList $ zip ws ([1..] :: [Integer])) {
        ppOutput = dbusOutput dbus
    }
    currentWorkspaceOnTop
    ewmhDesktopsLogHook
    fadeWindowsLogHook myFadeHook
    UP.updatePointer (UP.Relative 0.9 0.9)

myPP :: Show a => M.Map WorkspaceId a -> PP
myPP wmap = defaultPP
    { ppTitle    = pangoSpan [("foreground", "white"), ("font", "Cantarell 10")] . pangoSanitize
    , ppCurrent  = pangoColor "white" . wrap "[" "]"
                    . pangoSanitize . indexWorkspace
    , ppVisible  = pangoColor "yellow" . wrap "(" ")"
                    . pangoSanitize . indexWorkspace
    , ppHidden   = _hidden . noScratchPad
    , ppUrgent   = pangoColor "#FF0000"
                 . pangoSanitize . indexWorkspace
    , ppLayout   = pangoColor "lightblue" . pangoSanitize . shortenLayout
    , ppSep      = pangoColor "brown" $ pangoSanitize " : "
    , ppWsSep    = " "
    }
  where
    topicLength :: Integer
    topicLength = 3
    _hidden :: String -> String
    _hidden [] = ""
    _hidden x = pangoColor "#9a9a9a" . pangoSanitize
              . _shorten . indexWorkspace $ x

    _shorten :: String -> String
    _shorten ws = let m = ws =~ ("[0-9]+:.{0," ++ show topicLength ++ "}")
                  in if m == "" then ws else m

    indexWorkspace :: WorkspaceId -> WorkspaceId
    indexWorkspace w | w `M.member` wmap = show (wmap M.! w) ++ ":" ++ w
                     | otherwise         = w

    noScratchPad ws | ws =~ "^NSP(:[0-9]+)?$" = ""
                    | otherwise               = ws

    shortenLayout = shortenLayout' [
          ("^Tabbed\\s+(.*)", "T:\\1")
        , ("\\bThreeCol\\b", "OHH")
        , ("\\bThreeColMid\\b", "HOH")
        , ("\\bReflectX\\s+(.*)", "RX:\\1")
        , ("\\bReflectY\\s+(.*)", "RY:\\1")
        , ("\\bMirror\\s+(.*)", "M:\\1")
        , ("\\bResizableTall\\b", "OH")
        ]
    shortenLayout' [] s = s
    shortenLayout' ((reg, repl):xs) s = shortenLayout' xs
                                    $ subRegex (mkRegex reg) s repl

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal 
                (D.objectPath_ "/org/xmonad/Log")
                (D.interfaceName_ "org.xmonad.Log")
                (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<b>" ++ (pangoSpan [("font", "Cantarell Bold 10")] $ UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoSpan :: [(String, String)] -> String -> String
pangoSpan attrs = wrap left right
  where
    left = "<span " ++ attrstr ++ ">"
    right = "</span>"
    attrstr = intercalate " " $ fmap (\(x, y) -> x ++ "=\"" ++ y ++ "\"") attrs

pangoColor :: String -> String -> String
pangoColor fg = pangoSpan [("foreground", fg)]

--pangoColor2 :: String -> String -> String -> String
--pangoColor2 fg bg = pangoSpan [("foreground", fg), ("background", bg)]

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

-- | Enables 'focusFollowsMouse' for tiled windows only.  For this to
-- work you need to turn off 'focusFollowsMouse' in your configuration
-- and then add this function to your 'handleEventHook'.
focusFollowsTiledOnly :: Event -> X All
focusFollowsTiledOnly e@(CrossingEvent {ev_window = w, ev_event_type = t})
  | isNormalEnter = whenX bothTiled (focus w) >> mempty
  where isNormalEnter   = t == enterNotify && ev_mode e == notifyNormal
        bothTiled       = notFloating w <&&> currentIsTiled
        currentIsTiled  = currentWindow >>= maybe (return True) notFloating
        currentWindow   = gets $ W.peek . windowset
        notFloating w'  = gets $ not . M.member w' . W.floating . windowset
focusFollowsTiledOnly _ = mempty

myEventHook :: Event -> X All
myEventHook = mconcat
    [ ewmhDesktopsEventHook
    , docksEventHook
    , fadeWindowsEventHook
    , focusFollowsTiledOnly
    , fullscreenEventHook
    ]

myConfig dbus = myBaseConfig
    { modMask = myModMask
    , borderWidth = 1
    , normalBorderColor = "#FFD12B"
    , focusedBorderColor = "#FF511F"
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , layoutHook = desktopLayoutModifiers Local.layoutHook
    , keys = myKeys
    , logHook = myLogHook dbus
    , handleEventHook = myEventHook
    , manageHook = myManageHook
    , startupHook = myStartupHook
    , mouseBindings = myMouseBindings
    }
  where
    mc = myConfig dbus
    myStartupHook = do
        return () >> EZ.checkKeymap mc (emacsKeys mc)
        startupHook myBaseConfig
        -- adjustEventInput
        setWMName "LG3D"

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
    D.requestName dbus (D.busName_ "org.xmonad.Log")
            [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
        >> return ()

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ withUrgencyHook NoUrgencyHook $ myConfig dbus
