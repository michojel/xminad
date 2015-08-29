module XMonad.Local.Music where

import Control.Monad (liftM)

import qualified Network.MPD as MPD

toggleRepeat :: MPD.MonadMPD m => m()
toggleRepeat = do
    repeatState <- liftM MPD.stRepeat MPD.status 
    MPD.repeat $ not repeatState 

toggleRandom :: MPD.MonadMPD m => m()
toggleRandom = do
    randomState <- liftM MPD.stRandom MPD.status
    MPD.random $ not randomState
