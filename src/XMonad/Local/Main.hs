{-# LANGUAGE UnicodeSyntax             #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

-- This module is mainly a copy of upstream XMonad.Main.hs

module XMonad.Local.Main (
    myConfig,
    xminad,
    usage
) where

import           Control.Monad
import           Data.Function
import           System.Environment

import           XMonad
import           XMonad.Actions.UpdateFocus
import           XMonad.Config.Desktop
import qualified XMonad.Config.Mate as Mate
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Util.Run            (spawnPipe)

-- local modules **************************************************************
import qualified XMonad.Local.Config        as Local
import qualified XMonad.Local.EventHook     as Local
import qualified XMonad.Local.Keys          as Local
import qualified XMonad.Local.Layout        as Local
import qualified XMonad.Local.LogHook       as Local
import qualified XMonad.Local.ManageHook    as Local
import qualified XMonad.Local.Mouse         as Local
import qualified XMonad.Local.TopicSpace    as Local
import qualified XMonad.Local.XConfig       as Local

-- main module ****************************************************************
myConfig h = Local.xConfig
    { modMask = Local.modMask
    , borderWidth = 1
    , normalBorderColor = "#FFD12B"
    , focusedBorderColor = "#FF511F"
    , terminal = Local.terminal
    , workspaces = Local.workspaces
    , layoutHook = desktopLayoutModifiers Local.layoutHook
    , keys = Local.keyBindings
    , logHook = Local.logHook h
    , handleEventHook = Local.eventHook
    , manageHook = Local.manageHook
    , startupHook = myStartupHook
    , mouseBindings = Local.mouseBindings
    }
  where
    myStartupHook = do
        Mate.mateRegister
        startupHook Local.xConfig
        adjustEventInput
        setWMName "LG3D"

xminad ∷ IO ()
xminad = do
    installSignalHandlers -- important to ignore SIGCHLD to avoid zombies

    let launch' cmdArgs = do
                h <- spawnPipe "xmobar"
                let conf = withUrgencyHook NoUrgencyHook $ myConfig h
                conf'@XConfig{layoutHook = Layout l}
                    <- handleExtraArgs conf cmdArgs conf{layoutHook = Layout (layoutHook conf)}
                withArgs [] $ launch (conf' { layoutHook = l })

    cmdArgs <- getArgs
    case cmdArgs of
        ["--help"]          -> usage
        ["--restart"]       -> sendRestart
        "--replace" : args' -> sendReplace >> launch' args'
        _                   -> launch' cmdArgs

usage ∷ IO ()
usage = do
    self <- getProgName
    putStr . unlines $
        [concat ["Usage: ", self, " [OPTION]"],
        "Options:",
        "  --help           Print this message",
        "  --replace        Replace the running window manager with xminad",
        "  --restart        Request a running xminad process to restart"]

sendRestart ∷ IO ()
sendRestart = do
    dpy <- openDisplay ""
    rw <- rootWindow dpy $ defaultScreen dpy
    xmonad_restart <- internAtom dpy "XMONAD_RESTART" False
    allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw xmonad_restart 32 0 currentTime
        sendEvent dpy rw False structureNotifyMask e
    sync dpy False

-- | a wrapper for 'replace'
sendReplace ∷ IO ()
sendReplace = do
    dpy <- openDisplay ""
    let dflt = defaultScreen dpy
    rootw  <- rootWindow dpy dflt
    replace dpy dflt rootw

-- | @replace@ signals compliant window managers to exit.
replace ∷ Display → ScreenNumber → Window → IO ()
replace dpy dflt rootw = do
    -- check for other WM
    wmSnAtom <- internAtom dpy ("WM_S" ++ show dflt) False
    currentWmSnOwner <- xGetSelectionOwner dpy wmSnAtom
    when (currentWmSnOwner /= 0) $ do
        -- prepare to receive destroyNotify for old WM
        selectInput dpy currentWmSnOwner structureNotifyMask

        -- create off-screen window
        netWmSnOwner <- allocaSetWindowAttributes $ \attributes -> do
            set_override_redirect attributes True
            set_event_mask attributes propertyChangeMask
            let screen = defaultScreenOfDisplay dpy
                visual = defaultVisualOfScreen screen
                attrmask = cWOverrideRedirect .|. cWEventMask
            createWindow dpy rootw (-100) (-100) 1 1 0
                copyFromParent copyFromParent visual attrmask attributes

        -- try to acquire wmSnAtom, this should signal the old WM to terminate
        xSetSelectionOwner dpy wmSnAtom netWmSnOwner currentTime

        -- SKIPPED: check if we acquired the selection
        -- SKIPPED: send client message indicating that we are now the WM

        -- wait for old WM to go away
        fix $ \again -> do
            evt <- allocaXEvent $ \event -> do
                windowEvent dpy currentWmSnOwner structureNotifyMask event
                get_EventType event

            when (evt /= destroyNotify) again
