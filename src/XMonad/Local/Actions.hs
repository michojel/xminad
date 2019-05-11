{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE UnicodeSyntax   #-}

module XMonad.Local.Actions where

import           Control.Monad
import           Data.Maybe
import           System.Posix.Directory
import           System.Posix.Env
import           System.Posix.Signals         (Signal, signalProcess)
import qualified Turtle

import           XMonad
import qualified XMonad.Actions.TopicSpace    as TS
import qualified XMonad.Util.Paste            as Paste
import qualified XMonad.Util.Run              as Run
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

spawnTerm :: Maybe String -> Maybe FilePath -> Maybe String -> Maybe String
                          -> Maybe String -> X()
spawnTerm mprofile mwd mrole mtitle mcmd = do
    dir <- liftIO $ maybe (io $ return Nothing)
                   (fmap (Just . Turtle.encodeString) . Turtle.realpath . Turtle.decodeString)
	 				mwd
    Run.safeSpawn Local.terminal
        $ Local.mkTermArgs mprofile dir mrole mtitle mcmd

spawnShell ∷ X()
spawnShell = Run.safeSpawnProg Local.terminal

spawnInShell ∷ String → X()
spawnInShell = spawnTerm Nothing Nothing Nothing Nothing . Just

spawnShellIn ∷ TS.Dir → X()
spawnShellIn dir = spawnTerm Nothing (Just dir) Nothing Nothing Nothing

spawnInShellIn ∷ TS.Dir → String -> X()
spawnInShellIn dir cmd = spawnTerm Nothing (Just dir) Nothing Nothing (Just cmd)

spawnTmux ∷ String → X()
spawnTmux project = spawnInShell $ "tmux -c 'tmuxinator " ++ project ++ "'"

spawnSteamGameInWine :: Int -> X()
spawnSteamGameInWine gameId = Run.safeSpawn "wine"
        [ "C:/Program Files (x86)/Steam/Steam.exe"
        , "steam://rungameid/" ++ show gameId]

spawnSteamGame :: Int -> X()
spawnSteamGame gameId = Run.safeSpawn "steam" ["steam://rungameid/" ++ show gameId]

openURL :: String -> X()
openURL url = Run.safeSpawn Local.browser ["-n", url]

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
