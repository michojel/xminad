{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}

module XMonad.Layout.TopicDir (
    -- * Usage
    -- $usage
    topicDir,
    changeDir,
    TopicDir,
    ) where

import qualified Data.Map as M
import Control.Exception
import System.Directory (setCurrentDirectory, getCurrentDirectory)

import XMonad hiding (focus)
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Prompt (XPConfig)
import XMonad.Prompt.Directory (directoryPrompt)
import XMonad.Layout.LayoutModifier
import XMonad.StackSet (tag, currentTag)

econst :: Monad m => a -> IOException -> m a
econst = const . return

data Chdir = Chdir String deriving ( Typeable )
instance Message Chdir

data TopicDir a = TopicDir (M.Map WorkspaceId String, String)
   deriving (Read, Show)

instance LayoutModifier TopicDir Window where
    modifyLayout (TopicDir (tds, d)) w r = do
         tc <- gets (currentTag.windowset)
         case mdir tc of
            (Just dir) -> scd dir
            Nothing    -> return ()
         runLayout w r
      where
        mdir :: WorkspaceId -> Maybe String
        mdir tc | tc == tag w && d == "" = M.lookup tc tds
                | tc == tag w            = Just d
                | otherwise              = Nothing


    handleMess (TopicDir (dds, _)) m
        | Just (Chdir wd) <- fromMessage m = do
            wd' <- cleanDir wd
            return $ Just $ TopicDir (dds, wd')
        | otherwise = return Nothing

topicDir :: M.Map WorkspaceId String -> l a -> ModifiedLayout TopicDir l a
topicDir m = ModifiedLayout (TopicDir (m, ""))

cleanDir :: String -> X String
cleanDir x = scd x >> io getCurrentDirectory

scd :: String -> X ()
scd x = do
    x' <- io (runProcessWithInput "bash" [] ("echo -n " ++ x) `catch` econst x)
    catchIO $ setCurrentDirectory x'

changeDir :: XPConfig -> X ()
changeDir c = directoryPrompt c "Set working directory: " (sendMessage . Chdir)

