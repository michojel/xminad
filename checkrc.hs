--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import qualified Data.Map as M
import qualified Data.Set as S
import XMonad
import XMonad.StackSet (new)
import XMonad.Util.EZConfig (checkKeymap)

import XMonad.Local.XConfig
import XMonad.Local.Keys (emacsKeys)

--------------------------------------------------------------------------------
main :: IO ()
main = do
    dpy    <- openDisplay ""
    rootw  <- rootWindow dpy $ defaultScreen dpy

    let xmc = xConfig {layoutHook = Layout $ layoutHook xConfig}
        initialWinset = new (layoutHook xmc) (workspaces xmc) []

    let cf = XConf { display       = dpy
                   , config        = xmc
                   , theRoot       = rootw
                   , normalBorder  = 0
                   , focusedBorder = 0
                   , keyActions    = M.empty
                   , buttonActions = M.empty
                   , mouseFocused  = False
                   , mousePosition = Nothing
                   , currentEvent  = Nothing
                   }

    let st = XState { windowset       = initialWinset
                    , numberlockMask  = 0
                    , mapped          = S.empty
                    , waitingUnmap    = M.empty
                    , dragging        = Nothing
                    , extensibleState = M.empty
                    }

    -- Check key bindings, errors go to xmessage.
    _ <- runX cf st $ checkKeymap xmc (emacsKeys xmc)
    return ()
