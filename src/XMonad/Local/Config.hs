{-# LANGUAGE UnicodeSyntax #-}

module XMonad.Local.Config where

import qualified XMonad.Prompt   as P
import qualified XMonad.StackSet as W

explorer ∷ String
explorer = "caja"

terminal ∷ String
terminal = "st"

xpConfig ∷ P.XPConfig
xpConfig = P.def
    { P.fgColor = "#dfdfdf"
    , P.bgColor = "#3c3c3c"
    , P.fgHLight = "#ffffff"
    , P.bgHLight = "#3c3c3c"
    , P.font    = "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*"
    , P.height  = 24
    }

browser ∷ String
browser = "chrome-launcher"

vimBundlePath ∷ String
vimBundlePath = "~/.vim/bundle/vundle"

tabsOutlinerAppID ∷ String
tabsOutlinerAppID = "eggkanocgddhmamlbiijnphhppkpkmkl"
tabsOutlinerAppName ∷ String
tabsOutlinerAppName = "crx_" ++ tabsOutlinerAppID
tabsOutlinerFloatRect ∷ W.RationalRect
tabsOutlinerFloatRect = W.RationalRect (1/5) (1/11) (3/5) (7/9)
