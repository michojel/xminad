{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE UnicodeSyntax   #-}

module XMonad.Local.Util where

import Codec.Binary.UTF8.String
import Control.Monad
import System.IO
import System.Process           (runInteractiveProcess)

import XMonad

-- utilities ******************************************************************
runProcessAndGetOutputs ∷ MonadIO m ⇒ FilePath → [String] → String → m (String, String)
runProcessAndGetOutputs cmd args input = io $ do
    (pin, pout, perr, _) <- runInteractiveProcess (encodeString cmd) (map encodeString args) Nothing Nothing
    hPutStr pin input
    hClose pin
    output <- hGetContents pout
    when (output == output) $ return ()
    err <- hGetContents perr
    when (err == err) $ return ()
    hClose pout
    hClose perr
    -- no need to waitForProcess, we ignore SIGCHLD
    return (output, err)

runProcessAndLogError ∷ MonadIO m ⇒ FilePath → [String] → String → m (Maybe String)
runProcessAndLogError cmd args input = do
    (out, err) <- runProcessAndGetOutputs cmd args input
    if null err then
        return (Just out)
    else do
        io $ hPrint stderr $ "failed to run " ++ cmd ++ ": " ++ err
        return Nothing
