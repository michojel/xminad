module XMonad.Local.GridSelect (
      gsConfig
    , gsw
    , gswShift
    ) where

import Control.Monad
import Data.List (subsequences)
import XMonad
import qualified Data.Map as M
import XMonad.Actions.GridSelect
import qualified XMonad.Actions.TopicSpace as TS
import qualified XMonad.StackSet as W

-- local modules **************************************************************
import qualified XMonad.Local.TopicSpace as Local


gsConfig :: HasColorizer a => GSConfig a
gsConfig = defaultGSConfig
    { gs_cellheight = 40
    , gs_cellwidth = 100
    , gs_navigate = navigation'
    }
  where
    navigation' :: TwoD a (Maybe a)
    navigation' = makeXEventhandler
                $ shadowWithKeymap navKeyMap navHandler

    navKeyMap = M.fromList (allowModifs modifs
          [ ((0, xK_Escape)  , cancel)
          , ((0, xK_Return)  , select)
          , ((0, xK_slash)   , substringSearch navigation')
          , ((0, xK_question), substringSearch navigation')
          , ((0, xK_Left)    , move (-1, 0)  >> navigation')
          , ((0, xK_h)       , move (-1, 0)  >> navigation')
          , ((0, xK_H)       , move (-1, 0)  >> navigation')
          , ((0, xK_Right)   , move (1 , 0)  >> navigation')
          , ((0, xK_l)       , move (1 , 0)  >> navigation')
          , ((0, xK_L)       , move (1 , 0)  >> navigation')
          , ((0, xK_Down)    , move (0 , 1)  >> navigation')
          , ((0, xK_j)       , move (0 , 1)  >> navigation')
          , ((0, xK_J)       , move (0 , 1)  >> navigation')
          , ((0, xK_Up)      , move (0 , -1) >> navigation')
          , ((0, xK_k)       , move (0 , -1) >> navigation')
          , ((0, xK_K)       , move (0 , -1) >> navigation')
          , ((0, xK_n)       , moveNext      >> navigation')
          , ((0, xK_N)       , moveNext      >> navigation')
          , ((0, xK_p)       , movePrev      >> navigation')
          , ((0, xK_P)       , movePrev      >> navigation')
          ]
        ++ allowModifs (drop 1 modifs)
          [ ((0, xK_Tab)        , moveNext >> navigation')
          , ((shiftMask, xK_Tab), moveNext >> navigation')
          ]
        )
    modifs :: [KeyMask]
    modifs = [ shiftMask, lockMask, mod1Mask, mod2Mask
             , mod3Mask, mod4Mask, mod5Mask ]

    allowModifs :: [ KeyMask ] -> [((KeyMask, a), b)] -> [((KeyMask, a), b)]
    allowModifs mods keymap =
        [ ((m .|. o, k), a)
        | m <- map (foldl (.|.) 0) $ subsequences mods
        , ((o, k), a) <- keymap
        ]

    -- The navigation handler ignores unknown key symbols
    navHandler = const navigation'

gsw :: X()
gsw = gsw' W.greedyView
  where
    gsw' :: (WorkspaceId -> WindowSet -> WindowSet) -> X ()
    gsw' viewFunc = withWindowSet $ \ws -> do
        let wss = map W.tag $ fHidden ws ++ map W.workspace (W.current ws : W.visible ws)
        gridselect gsConfig (zip wss wss) >>= flip whenJust (switchTopic' viewFunc)
    fHidden = filter ((/=) "NSP" . W.tag) . W.hidden

gswShift :: X()
gswShift = gridselectWorkspace gsConfig (\ws -> W.greedyView ws . W.shift ws)

switchTopic' :: (WorkspaceId -> WindowSet -> WindowSet)
                -> TS.Topic -> X ()
switchTopic' viewMethod topic = do
   windows $ viewMethod topic
   wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
   when (null wins) $ TS.topicAction Local.topicConfig topic
