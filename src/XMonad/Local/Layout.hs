{-# OPTIONS -fno-warn-missing-signatures #-}

module XMonad.Local.Layout (layoutHook) where

import Data.Ratio ((%))
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.Accordion
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Column
import qualified XMonad.Layout.ComboP as CP
import qualified XMonad.Layout.IM as IM
import qualified XMonad.Layout.MultiToggle as MT
import qualified XMonad.Layout.Named as LN
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.PerWorkspace as PW
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.SubLayouts
import qualified XMonad.Layout.Tabbed as Tab
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

-- local modules **************************************************************
import XMonad.Layout.TopicDir as TD
import XMonad.Local.TopicSpace as Local

layoutHook = avoidStruts
             $ TD.topicDir Local.topicDirs
             $ PW.onWorkspace "chat" chatL
             $ PW.onWorkspace "gimp" gimpL
             $ PW.onWorkspace "BG" bgL
             $ PW.onWorkspace "remote" remoteL
             $ PW.onWorkspaces ["homam5", "civ4", "pst", "ciV"] wineGameL
             $ easyLay

nmaster = 1
ratio = 1/2
delta = 3/100

tiled =  Tall nmaster delta ratio
threecol =  ThreeColMid nmaster delta (1/3)

-- common layouts
easyLay = windowNavigation baseLay 
baseLay = smartBorders $ (mySubTabbed $ BW.boringWindows $
          toggleLayouts threecol
                 (   MT.mkToggle (MT.single REFLECTX) tiled
                 ||| MT.mkToggle (MT.single REFLECTY) (Mirror tiled)))
         ||| (BW.boringWindows $ trackFloating $ Tab.tabbed Tab.shrinkText myTabTheme)

-- workspace layouts
chatL = IM.withIM (1%5) (IM.ClassName "Skype"
             `IM.And`   (        IM.Title "minarmc - Skype™ (Beta)"
                        `IM.Or`  IM.Title "Skype™ 2.2 (Beta) for Linux"
                        `IM.Or`  IM.Title "minarmc - Skype™"))
      $ IM.withIM (1%5) (        IM.ClassName "Empathy"
                        `IM.And` (IM.Title "Contact List" `IM.Or` IM.Role "contact_list"))
      $ easyLay

gimpL = LN.named "GIMP"
      $ windowNavigation
      $ smartBorders
      $ IM.withIM (11/64) (IM.Role "gimp-toolbox")
      $ CP.combineTwoP
            (reflectHoriz $ TwoPane delta 0.2)
            (Column 0)
            (mySubTabbed $ BW.boringWindows Accordion)
            (        CP.ClassName "Gimp"
            `CP.And` CP.Not (CP.Role "gimp-image-window"))

bgL = windowNavigation $ BW.boringWindows $ smartBorders
    $ reflectHoriz $ Tall nmaster delta (7/9)

remoteL = windowNavigation $ BW.boringWindows $ smartBorders
        $ Tab.tabbed Tab.shrinkText myTabTheme

wineGameL = smartBorders $ simpleFloat ||| trackFloating Full

mySubTabbed x = trackFloating $ Tab.addTabs Tab.shrinkText myTabTheme $ subLayout [] Simplest x

myTabTheme :: Tab.Theme
myTabTheme = Tab.defaultTheme
    { Tab.activeTextColor     = "#ffffff"
    , Tab.activeBorderColor   = "#FBAB2E"
    , Tab.activeColor         = "#3c3c3c"
    , Tab.inactiveTextColor   = "#c0c0c0"
    , Tab.inactiveBorderColor = "#c0c0c0"
    , Tab.inactiveColor       = "#3c3c3c"
    , Tab.urgentTextColor     = "#ff0000"
    , Tab.urgentBorderColor   = "#ff0000"
    , Tab.urgentColor         = "#000000"
    , Tab.fontName            = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
    , Tab.decoHeight          = 24
    }
