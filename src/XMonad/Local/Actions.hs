module XMonad.Local.Actions where

import XMonad
import qualified XMonad.Actions.TopicSpace as TS

myExplorer :: String
myExplorer = "caja"

spawnExplorer :: MonadIO m => m ()
spawnExplorer = spawn myExplorer

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
