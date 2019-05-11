{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module XMonad.Local.Operations (restart) where

import           System.Directory
import           System.Environment
import           System.IO
#ifdef NIXOS_USER_BINARY
import           Turtle hiding (stderr)
#else
import           System.FilePath.Posix ((</>))
#endif

import qualified XMonad                as X
import qualified XMonad.Operations     as Op

#ifndef NIXOS_USER_BINARY
import           Paths_xminad          (getBinDir)
#endif

restart ∷ X.X ()
restart = do
    binPath <- X.io $ do
#ifdef NIXOS_USER_BINARY
        -- TODO: find out how to pass the path with -D cpp-option
        binaryPath <- fmap
                (encodeString . (\h -> h </> decodeString ".nix-profile/bin/xminad"))
                Turtle.home 
#else
        bindir <- getBinDir
        let binaryPath = bindir </> "xminad"
#endif
        exists <- doesFileExist binaryPath
        if exists
            then do
                hPutStr stderr $ "reloading binary at " ++ binaryPath ++ "...\n"
                return binaryPath
            else do
                pth <- getExecutablePath
                hPutStr stderr $ "binary path " ++ binaryPath ++ " does not exist, falling back to " ++ pth ++ "\n"
                return pth
    Op.restart binPath True
