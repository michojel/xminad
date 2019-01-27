{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE UnicodeSyntax   #-}

module XMonad.Local.Actions where

import           Control.Monad
import           Data.Maybe
import           System.Posix.Directory
import           System.Posix.Env
import           System.Posix.Signals         (Signal, signalProcess)

import           XMonad
import qualified XMonad.Actions.TopicSpace    as TS
import qualified XMonad.Util.Paste            as Paste
import qualified XMonad.Util.WindowProperties as WP

-- local modules **************************************************************
import qualified XMonad.Local.Config          as Local
import qualified XMonad.Local.Prompt          as Local
import qualified XMonad.Local.Util            as Local

-- launch applications ********************************************************
spawnExplorer ∷ X ()
spawnExplorer = do
    cwd <- liftIO getWorkingDirectory
    pth <- liftIO $ getEnvDefault "HOME" cwd
    spawnExplorerIn pth

spawnExplorerIn ∷ String → X ()
spawnExplorerIn dir = spawn $ Local.explorer ++ " " ++ dir

spawnShell ∷ Maybe String → X()
spawnShell = spawnShellIn ""

spawnShellIn ∷ TS.Dir → Maybe String → X()
spawnShellIn dir command = do
    t <- asks (terminal . config)
    spawn $ cmd' t
  where
    -- TODO: escape quotes in the c command
    run (Just c) = " -e '" ++ c ++ "'"
    run Nothing  = ""

    cmd' t | dir == "" = t ++ run command
           | otherwise = "cd " ++ dir ++ " && " ++ t ++ run command

spawnTmux ∷ String → X()
spawnTmux project = spawnShell $ Just ("tmux -c 'tmuxinator " ++ project ++ "'")

killWindowPID ∷ Signal → Window → X()
killWindowPID s w = do
    pid <- WP.getProp32s "_NET_WM_PID" w
    when (isJust pid) (liftIO $ mapM_ (signalProcess s . fromIntegral) (fromJust pid))

signalCurrentWindow ∷ Signal → X()
signalCurrentWindow s = withFocused (killWindowPID s)

clipboardManager ∷ String
clipboardManager = "xsel"

getClipboardText ∷ X (Maybe String)
getClipboardText = catchX
    (Local.runProcessAndLogError clipboardManager ["-b"] "")
    (io $ return Nothing)

saveTextToClipboard ∷ String → X ()
saveTextToClipboard text = catchX
    (void (Local.runProcessAndLogError clipboardManager ["-b"] text))
    (io $ return ())

getAndPasteDigraph ∷ X ()
getAndPasteDigraph = do
    dg <- Local.digraphPrompt
    case dg of
        Just text@(_:_) -> do
            saveTextToClipboard text
            -- TODO: support terminal emulators (Shift+Insert)
            -- determine by wm_class
            Paste.pasteChar controlMask 'V'
        _               -> io $ return ()

-- | Turn the current entry in clipboard into plain text and paste it to the
-- current window using Ctrl+v shortcut.
pastePlainTextFromClipboard ∷ X ()
pastePlainTextFromClipboard = do
    t <- getClipboardText
    case t of
        Just text@(_:_) -> do
            saveTextToClipboard text
            Paste.pasteChar controlMask 'V'
        _               -> io $ return ()
