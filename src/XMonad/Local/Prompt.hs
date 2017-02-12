{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE UnicodeSyntax   #-}

module XMonad.Local.Prompt (
    digraphPrompt,
    getDigraphForString
) where

import           Control.Exception
import           Control.Monad
import           System.Directory
import           System.IO
import qualified System.IO.Temp      as Temp

import           XMonad
import           XMonad.Prompt

-- local modules **************************************************************
import qualified XMonad.Local.Config as Local
import qualified XMonad.Local.Util   as Local

-- digraph prompt *************************************************************
data Digraph = Digraph

instance XPrompt Digraph where
    showXPrompt Digraph = "Two ASCII characters: "

-- | According to RFC 1345
digraphChars ∷ String
digraphChars = "!\"%'()*+,-./0123456789:;<=>?"
            ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

digraphCompletionFunction ∷ String → IO [String]
digraphCompletionFunction = return . gen
    where
        gen ∷ String → [String]
        gen []                                  = [x:"?" | x <- digraphChars]
        gen [x]    | isValid 0 x                = [[x, y] | y <- '_':digraphChars]
                   | otherwise                  = []
        gen [x, y] | isValid 0 x && isValid 1 y = [[x, y]]
                   | otherwise                  = []
        gen _                                   = []

        isValid :: Int -> Char -> Bool
        isValid 0 c = c `elem` digraphChars
        isValid 1 c = c `elem` ('_':digraphChars)
        isValid _ _ = False

vimEchoDigraphConfig ∷ String
vimEchoDigraphConfig = unlines [
    "set noswapfile",
    "set nocompatible",
    "filetype off",
    "\" load unicode plugin for unicode#Digraph function",
    "set rtp+=" ++ Local.vimBundlePath,
    "call vundle#begin()",
    "Plugin 'chrisbra/unicode.vim'",
    "call vundle#end()"]

vimBinary ∷ String
vimBinary = "/usr/bin/vim"

-- |
-- TODO: make it vim independent
getDigraphForString ∷ String → X String
getDigraphForString str = io $
    Temp.withSystemTempFile "xminad-digraph-config.vimrc" $ \filePath h ->
        finally (runVim filePath h) (removeFile filePath)
    where
        cmdMkDigraph = "+call setline(1, unicode#Digraph(getline(1)[0:1]))"
        cmdWriteAndQuit = "+w!/dev/stderr|q!"

        runVim ∷ FilePath → Handle → IO String
        runVim filePath h = do
            finally (hPutStr h vimEchoDigraphConfig) (hClose h)
            (_, dg) <- Local.runProcessAndGetOutputs vimBinary
                ["-", "-esbu", filePath, cmdMkDigraph, cmdWriteAndQuit]
                str
            when (null dg) (hPrint stderr $ "failed to get digraph for input '" ++ str ++ "'\n")
            return dg

-- | Prompts for two characters and passes them to vim to get corresponding
-- digraph.
-- NOTE: Even if return value is Just, the string may still be empty if the
-- vim couldn't make digraph out of the input.
digraphPrompt ∷ X (Maybe String)
digraphPrompt = catchX
     (mkXPromptWithReturn Digraph
        Local.xpConfig digraphCompletionFunction getDigraphForString)
     (io $ return Nothing)
