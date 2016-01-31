{-# LANGUAGE DoAndIfThenElse #-}

module XMonad.Local.Actions where

import Control.Monad
import Data.Maybe
import System.Posix.Directory
import System.Posix.Env
import System.Posix.Signals (Signal, signalProcess)

import XMonad
import qualified XMonad.Actions.TopicSpace as TS
import qualified XMonad.Util.WindowProperties as WP

-- local modules **************************************************************
import qualified XMonad.Local.Config as Local

-- launch applications ********************************************************
spawnExplorer :: X ()
spawnExplorer = do 
    cwd <- liftIO getWorkingDirectory
    pth <- liftIO $ getEnvDefault "HOME" cwd
    spawnExplorerIn pth
    
spawnExplorerIn :: String -> X ()
spawnExplorerIn dir = spawnShellIn dir (Just Local.explorer)

spawnShell :: Maybe String -> X()
spawnShell = spawnShellIn ""

spawnShellIn :: TS.Dir -> Maybe String -> X()
spawnShellIn dir command = do
    t <- asks (terminal . config)
    spawn $ cmd' t
  where
    run (Just c) = " -e '" ++ c ++ "'"
    run Nothing  = ""

    cmd' t | dir == "" = t ++ run command
           | otherwise = "cd " ++ dir ++ " && " ++ t ++ run command

killWindowPID :: Signal -> Window -> X()
killWindowPID s w = do
    pid <- WP.getProp32s "_NET_WM_PID" w
    when (isJust pid) (liftIO $ mapM_ (signalProcess s . fromIntegral) (fromJust pid))

signalCurrentWindow :: Signal -> X()
signalCurrentWindow s = withFocused (killWindowPID s)

mateRun :: X ()
mateRun = withDisplay $ \dpy -> do
    rw <- asks theRoot
    mate_panel <- getAtom "_MATE_PANEL_ACTION"
    panel_run  <- getAtom "_MATE_PANEL_ACTION_RUN_DIALOG"

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw mate_panel 32 panel_run 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False
