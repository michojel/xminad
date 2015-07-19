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
import qualified XMonad.Layout.MouseResizableTile as MRT
import qualified XMonad.Layout.MultiToggle as MT
import qualified XMonad.Layout.Named as LN
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.PerWorkspace as PW
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.StackTile
import XMonad.Layout.SubLayouts
import qualified XMonad.Layout.Tabbed as Tab
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

-- local modules **************************************************************
import XMonad.Layout.TopicDir as TD
import XMonad.Local.TopicSpace as Local

myTabTheme :: Tab.Theme
myTabTheme = Tab.defaultTheme
    { Tab.activeTextColor = "#ffffff"
    , Tab.activeBorderColor = "#FBAB2E"
    , Tab.activeColor = "#3c3c3c"
    , Tab.inactiveTextColor = "#c0c0c0"
    , Tab.inactiveBorderColor = "#c0c0c0"
    , Tab.inactiveColor = "#3c3c3c"
    , Tab.urgentTextColor = "#ff0000"
    , Tab.urgentBorderColor = "#ff0000"
    , Tab.urgentColor = "#000000"
    , Tab.fontName = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
    , Tab.decoHeight = 24
    }

layoutHook = avoidStruts
             $ TD.topicDir Local.topicDirs
             $ PW.onWorkspace "chat" chatL
             $ PW.onWorkspace "gimp" gimpL
             $ PW.onWorkspace "BG" bgL
             $ PW.onWorkspace "remote" remoteL
             $ PW.onWorkspaces ["homam5", "civ4", "pst", "ciV"] wineGameL
             $ _easyLay
  where
    -- basic layouts
    _tiled = Tall nmaster delta ratio
    _threecol = ThreeColMid nmaster delta (1/3)
    _stack = StackTile nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100
    _mrt = MRT.mouseResizableTile
             { MRT.draggerType = MRT.FixedDragger
                { MRT.gapWidth = 2, MRT.draggerWidth = 2 }
             }
    _mrt2 = MRT.mouseResizableTile
             { MRT.masterFrac = 0.8
             , MRT.fracIncrement = delta
             , MRT.draggerType = MRT.BordersDragger
             }

    -- common layouts
    _easyLay = windowNavigation _baseLay 
    _baseLay = smartBorders $ (mySubTabbed $ BW.boringWindows $ toggleLayouts _threecol
                     (   MT.mkToggle (MT.single REFLECTX) _tiled
                     ||| MT.mkToggle (MT.single REFLECTY) (Mirror _tiled)))
             ||| (BW.boringWindows $ Tab.tabbed Tab.shrinkText myTabTheme)

    -- workspace layouts
    chatL = IM.withIM (1%5) (IM.ClassName "Skype"
                 `IM.And`   (        IM.Title "minarmc - Skype™ (Beta)"
                            `IM.Or`  IM.Title "Skype™ 2.2 (Beta) for Linux"
                            `IM.Or`  IM.Title "minarmc - Skype™"))
          $ IM.withIM (1%5) (        IM.ClassName "Empathy"
                            `IM.And` (IM.Title "Contact List" `IM.Or` IM.Role "contact_list"))
          {--
          $ IM.withIM (1%5) (        IM.ClassName "Pidgin"
                            `IM.And` IM.Role "buddy_list")
          --}
          {--
          $ IM.withIM (1%5) (        IM.ClassName "Google-chrome"
                            `IM.And` IM.Title "Hangouts")
          --}
          $ _easyLay

    gimpL = LN.named "GIMP"
          -- $ configurableNavigation noNavigateBorders $ BW.boringWindows
          $ windowNavigation
          $ smartBorders
          $ IM.withIM (11/64) (IM.Role "gimp-toolbox")
          $ CP.combineTwoP
                (reflectHoriz $ TwoPane delta 0.2)
                (Column 0)
                (mySubTabbed $ BW.boringWindows Accordion)
                (        CP.ClassName "Gimp"
                `CP.And` CP.Not (CP.Role "gimp-image-window"))

    bgL = windowNavigation $ BW.boringWindows $ smartBorders $ reflectHoriz $ Tall nmaster delta (7/9)

    remoteL = windowNavigation $ BW.boringWindows $ smartBorders $ Tab.tabbed Tab.shrinkText myTabTheme

    wineGameL = smartBorders $ simpleFloat ||| Full

    mySubTabbed x = Tab.addTabs Tab.shrinkText myTabTheme $ subLayout [] Simplest x