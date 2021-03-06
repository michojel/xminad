{-# LANGUAGE UnicodeSyntax #-}

module XMonad.Local.Config where

import qualified Data.ByteString.UTF8 as BSU
import Data.List                    (isPrefixOf)
import Text.ShellEscape             (escape)

import qualified XMonad.Prompt   as P
import qualified XMonad.StackSet as W

explorer ∷ String
explorer = "pcmanfm-qt"

terminal ∷ String
terminal = "terminator"

-- class parameter does not work for terminator
mkTermArgs :: Maybe String -> Maybe FilePath -> Maybe String -> Maybe String 
                          -> Maybe String -> [String]
mkTermArgs mprofile mwd mrole mtitle mcmd = foldr combine [] args
  where
    args = [ ("-p", mprofile)
           , ("--working-directory", mwd)
           , ("-r", mrole)
           , ("-T", mtitle)
           , ("-e", mcmd)
           ]
    combine :: (String, Maybe String) -> [String] -> [String]
    combine (_, Nothing) b       = b
    combine (flag, Just value) b = b ++ mkArg flag value
    mkArg flag value | "--" `isPrefixOf` flag = [flag ++ "=" ++ value]
                     | otherwise              = [flag, value]

mkTermCmd :: Maybe String -> Maybe FilePath -> Maybe String -> Maybe String 
                          -> Maybe String -> String
mkTermCmd mprofile mwd mrole mtitle mcmd = unwords
        $ map ((\t -> "\"" ++ t ++ "\"") . BSU.toString . escape . BSU.fromString)
        $ terminal : mkTermArgs mprofile mwd mrole mtitle mcmd

xpConfig ∷ P.XPConfig
xpConfig = P.def
    { P.fgColor  = "#dfdfdf"
    , P.bgColor  = "#3c3c3c"
    , P.fgHLight = "#ffffff"
    , P.bgHLight = "#3c3c3c"
    , P.font     = "xft:Ubuntu Mono:size=10"
    , P.height   = 24
    }

browser ∷ String
browser = "w3"

vimBundlePath ∷ String
vimBundlePath = "~/.config/nvim/plugged"

tabsOutlinerAppID ∷ String
tabsOutlinerAppID = "eggkanocgddhmamlbiijnphhppkpkmkl"
tabsOutlinerAppName ∷ String
tabsOutlinerAppName = "crx_" ++ tabsOutlinerAppID
tabsOutlinerFloatRect ∷ W.RationalRect
tabsOutlinerFloatRect = W.RationalRect (1/5) (1/11) (3/5) (7/9)

remoteDesktopAppID ∷ String
remoteDesktopAppID = "gbchcmhmhahfdphkhkmpfmihenigjmpp"
remoteDesktopAppName ∷ String
remoteDesktopAppName = "crx_" ++ remoteDesktopAppName

chromeClassNames ∷ [String]
chromeClassNames = ["Chromium", "Google-chrome", "google-chrome", "Chromium-browser"]

screenSaverCmd ∷ String
screenSaverCmd = "xdg-screensaver"
