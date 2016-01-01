module XMonad.Local.Config (
      explorer
    , xpConfig
    , terminal
    ) where

import qualified XMonad.Prompt as P

explorer :: String
explorer = "caja"

terminal :: String
terminal = "mate-terminal"

xpConfig :: P.XPConfig
xpConfig = P.def
    { P.fgColor = "#dfdfdf"
    , P.bgColor = "#3c3c3c"
    , P.fgHLight = "#ffffff"
    , P.bgHLight = "#3c3c3c"
    , P.font    = "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*"
    , P.height  = 24
    }

