{-# LANGUAGE UnicodeSyntax #-}

module XMonad.Local.Operations (restart) where

import           System.Directory
import           System.Environment
import           System.FilePath.Posix ((</>))

import qualified XMonad                as X
import qualified XMonad.Operations     as Op

import           Paths_xminad          (getBinDir)

restart ∷ X.X ()
restart = do
    binPath <- X.io $ do
        bindir <- getBinDir
        let binaryPath = bindir </> "xminad"
        exists <- doesFileExist binaryPath
        if exists
            then return binaryPath
            else getExecutablePath
    Op.restart binPath True
