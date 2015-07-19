{-# LANGUAGE DoAndIfThenElse #-}

module XMonad.Local.Actions where

import XMonad
import qualified XMonad.Actions.TopicSpace as TS

-- local modules **************************************************************
import qualified XMonad.Local.Config as Local

-- launch applications ********************************************************
spawnExplorer :: MonadIO m => m ()
spawnExplorer = spawn Local.explorer

spawnShell :: Maybe String -> X()
spawnShell = spawnShellIn ""

spawnShellIn :: TS.Dir -> Maybe String -> X()
spawnShellIn dir command = do
    t <- asks (terminal . config)
    spawn $ cmd' t
  where
    run (Just c) = " -e '" ++ c ++ "'"
    run Nothing    = ""

    cmd' t | dir == "" = t ++ run command
           | otherwise = "cd " ++ dir ++ " && " ++ t ++ run command

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
