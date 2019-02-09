{-# LANGUAGE DoAndIfThenElse #-}

module XMonad.Local.Workspaces (
      newWorkspace
    , newWorkspaceDir
    , promptedNewWorkspace
    , swapScreens
    , toggleWS
    , toggleWSSwitch
    ) where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import XMonad
import qualified XMonad.Actions.CycleWS as WS
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Actions.TopicSpace as TS
import qualified XMonad.Layout.TopicDir as TD
import qualified XMonad.Prompt.Input as PI
import qualified XMonad.StackSet as W

-- local modules **************************************************************
import qualified XMonad.Local.Config as Local
import qualified XMonad.Local.TopicSpace as Local

-- workspace creation *********************************************************
promptedNewWorkspace :: Bool -> X()
promptedNewWorkspace shiftFocused = PI.inputPrompt Local.xpConfig "New Workspace"
                                  PI.?+ action shiftFocused
  where
    action :: Bool -> String -> X()
    action True = newWorkspaceDir shiftto
    action _    = newWorkspaceDir goto

{-
getPromptedNewWorkspace :: X (Maybe WorkspaceId)
getPromptedNewWorkspace = PI.inputPrompt Local.xpConfig "New Workspace"
                                  PI.?+ action shiftFocused
  where
    action :: Bool -> String -> X()
    action True = newWorkspaceDir shiftto
    action _    = newWorkspaceDir goto
-}

newWorkspace :: WorkspaceId -> X()
newWorkspace w = do
    exists <- widExists w
    unless exists $ DW.addHiddenWorkspace w

newWorkspaceDir :: (TS.Topic -> X()) -> WorkspaceId -> X()
newWorkspaceDir gotofunc w = do
    exists <- widExists w
    if not exists then do
        DW.addHiddenWorkspace w
        gotofunc w
        unless (w `S.member` doNotAskForDir) $ TD.changeDir Local.xpConfig
    else
        gotofunc w
  where
    doNotAskForDir :: S.Set WorkspaceId
    doNotAskForDir = S.fromList $
         ["mail", "chat", "virt", "vbox", "web"] ++ M.keys Local.topicDirs

widExists :: WorkspaceId -> X Bool
widExists wid = widExists' wid . windowset <$> get
  where
    widExists' :: WorkspaceId -> W.StackSet WorkspaceId l a s sd -> Bool
    widExists' w ws = w `elem` map W.tag (W.workspaces ws)

-- workspace switching ********************************************************
toggleWS :: X()
toggleWS = do
    hs' <- cleanHiddens ["NSP"]
    unless (null hs') (windows . W.greedyView . W.tag $ head hs')
  where
    cleanHiddens :: [WorkspaceId] -> X [WindowSpace]
    cleanHiddens skips = gets $ flip WS.skipTags skips . W.hidden . windowset

toggleWSSwitch :: X()
toggleWSSwitch = do
    hs' <- cleanHiddens []
    unless (null hs') (windows . (\ws -> W.greedyView ws . W.shift ws) . W.tag $ head hs')
  where
    cleanHiddens :: [WorkspaceId] -> X [WindowSpace]
    cleanHiddens skips =  gets $ flip WS.skipTags skips . W.hidden . windowset

-- creates the workspace if needed
goto :: TS.Topic -> X()
goto t = newWorkspace t >> TS.switchTopic Local.topicConfig t
shiftto :: TS.Topic -> X()
shiftto t = newWorkspace t >> windows (W.greedyView t . W.shift t)

-- swap workspaces between screens
swapScreens :: X ()
swapScreens = do
    screen <- gets (listToMaybe . W.visible . windowset)
    whenJust screen $ windows . W.greedyView . W.tag . W.workspace
