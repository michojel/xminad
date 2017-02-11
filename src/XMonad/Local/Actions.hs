{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE UnicodeSyntax   #-}

module XMonad.Local.Actions where

import           Codec.Binary.UTF8.String
import           Control.Monad
import           Data.Maybe
import           System.IO
import           System.Posix.Directory
import           System.Posix.Env
import           System.Posix.Signals         (Signal, signalProcess)
import           System.Process               (runInteractiveProcess)

import           XMonad
import qualified XMonad.Actions.TopicSpace    as TS
import qualified XMonad.Util.Paste            as Paste
import qualified XMonad.Util.WindowProperties as WP

-- local modules **************************************************************
import qualified XMonad.Local.Config          as Local

-- launch applications ********************************************************
spawnExplorer ∷ X ()
spawnExplorer = do
    cwd <- liftIO getWorkingDirectory
    pth <- liftIO $ getEnvDefault "HOME" cwd
    spawnExplorerIn pth

spawnExplorerIn ∷ String → X ()
spawnExplorerIn dir = spawn $ Local.explorer ++ " --no-desktop --browser " ++ dir

spawnShell ∷ Maybe String → X()
spawnShell = spawnShellIn ""

spawnShellIn ∷ TS.Dir → Maybe String → X()
spawnShellIn dir command = do
    t <- asks (terminal . config)
    spawn $ cmd' t
  where
    run (Just c) = " " ++ c
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

mateRun ∷ X ()
mateRun = withDisplay $ \dpy -> do
    rw <- asks theRoot
    mate_panel <- getAtom "_MATE_PANEL_ACTION"
    panel_run  <- getAtom "_MATE_PANEL_ACTION_RUN_DIALOG"

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw mate_panel 32 panel_run 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False

clipboardManager ∷ String
clipboardManager = "/usr/bin/clipit"

runProcessAndLogError ∷ MonadIO m ⇒ FilePath → [String] → String → m (Maybe String)
runProcessAndLogError cmd args input = io $ do
    (pin, pout, perr, _) <- runInteractiveProcess (encodeString cmd) (map encodeString args) Nothing Nothing
    hPutStr pin input
    hClose pin
    output <- hGetContents pout
    when (output == output) $ return ()
    err <- hGetContents perr
    when (err == err) $ return ()
    hClose pout
    hClose perr
    unless (null err) $ hPrint stderr $ "failed to run " ++ clipboardManager ++ ": " ++ err
    -- no need to waitForProcess, we ignore SIGCHLD
    return $ Just output

getClipboardText ∷ X (Maybe String)
getClipboardText = catchX
    (runProcessAndLogError clipboardManager ["-c"] "")
    (io $ return Nothing)

pasteTextFromClipboard ∷ X ()
pasteTextFromClipboard = do
    t <- getClipboardText
    case t of
        Just text -> catchX (Paste.pasteString text) (io $ return ())
        Nothing   -> io (return ())
