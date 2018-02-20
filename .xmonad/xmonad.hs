{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import XMonad
import Control.Monad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Hooks.SetWMName
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.EwmhDesktops

import System.IO
import XMonad.Util.Run
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.PerWorkspace
import XMonad.Actions.CycleWS
import Text.Printf
import Data.Aeson.Lens
import System.Process
import System.Exit
import Control.Lens
import Data.Maybe
import Control.Exception
import XMonad.Util.Run

myWorkspaces =
    [
        ("1", xK_1),
        ("2", xK_2),
        ("3", xK_parenleft),
        ("4", xK_dollar),
        ("5", xK_braceleft),
        ("6", xK_equal),
        ("pdf", xK_p),
        ("web", xK_w),
        ("video", xK_v),
        ("music", xK_m),
        ("chat", xK_c),
        ("graveyard", xK_g)
    ]

viewShift = doF . liftM2 (.) W.greedyView W.shift

myShifts = [ className =? "Chromium" --> doShift "web"
           , className =? "Spotify" --> doShift "music"
           , className =? "Zathura" --> viewShift "pdf"
           , className =? "Kodi" --> viewShift "video"
           ]
myFloats = [ className =? "MPlayer" --> doFloat
           , className =? "VirtualBox" --> doFloat
           ]
myManageHooks = composeAll $ myShifts ++ myFloats

myLayoutHook = onWorkspace "video" myFull $ myTall ||| myFull ||| simpleFloat
    where
        myTall = smartBorders $ Tall 1 (2/100) (1/2)
        myFull = noBorders $ Full

makeWorkspaceKeys :: ButtonMask -> [(String,KeySym)] -> [((ButtonMask,KeySym), X())]
makeWorkspaceKeys mask workspaces = gotoKeys workspaces ++ moveKeys workspaces
    where
        gotoKeys = map (\(name, key) -> ((mask, key), windows $ W.view name))
        moveKeys = map (\(name, key) -> ((mask .|. shiftMask, key), windows $ W.shift name))

keysToAdd x =
    makeWorkspaceKeys mod4Mask myWorkspaces
    ++ [ ((0, xF86XK_MonBrightnessUp), spawn "sudo /home/gustav/bin/xbacklight -inc 10")
       , ((0, xF86XK_MonBrightnessDown), spawn "sudo /home/gustav/bin/xbacklight -dec 10")
       , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 5%+")
       , ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 5%-")
       , ((0, xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle")
       , ((0, xF86XK_Display), spawn "/home/gustav/.local/bin/displayswitcheroo")
       , ((0, xK_F11), spawn "/home/gustav/bin/screenshot")
       , ((mod4Mask, xK_n), swapNextScreen)
       , ((mod4Mask, xK_t), nextScreen)
       , ((modMask x, xK_d), spawn "docs")
       ]

keysToRemove x =
    [
    ]

strippedKeys x = foldr M.delete (keys def x) (keysToRemove x)
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))

myDzenPP = def
  { ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
  , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
  , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
  , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
  , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
  , ppWsSep             =   " "
  , ppSep               =   "  |  "
  , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
  }

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)


-- modified version of https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/src/XMonad-Hooks-DynamicLog.html#statusBar
-- that does not do any ppOutput
statusBar' cmd pp k conf = do
    _ <- spawnPipe cmd
    return $ docks $ conf
        { layoutHook = avoidStruts (layoutHook conf)
        , logHook = do
            logHook conf
            dynamicLogWithPP pp { ppOutput = \_ -> return ()}
        , keys = liftM2 M.union keys' (keys conf)
        }
          where keys' = (`M.singleton` sendMessage ToggleStruts) . k

bars conf =
  (statusBar' myStatusBar myDzenPP toggleStrutsKey conf)
    >>= (statusBar myXmonadBar myDzenPP toggleStrutsKey)
    where (x, y, width) = (0, 0, 1920) :: (Int, Int, Int)
          left = 600
          right = width - left
          fontSize = 13 :: Int
          font = printf "-h '%d' -fn '-*-*-*-*-*-*-%d-*-*-*-*-*-*-*'" (fontSize + 2) fontSize :: String
          dzen = "dzen2 -e 'onstart=lower' -dock" :: String
          myXmonadBar = printf "%s -x '0' -y '0' -w '%d' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E' %s" dzen left font
          myStatusBar = printf "conky -c ~/.xmonad/conky_dzen 0>/dev/null | %s -x '%d' -y '0' -w '%d' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' %s" dzen left right font

myStartupHook = composeAll [ setWMName "LG3D"
                           , spawnOn "music" "spotify"
                           , spawnOn "chat" "urxvt -e drop"
                           , spawnOn "chat" "urxvt -e mail"
                           , spawnOn "2" "urxvt -e tmx hack"
                           , spawnOn "pdf" "urxvt -e docs"
                           ]

main :: IO ()
main = xmonad =<< bars (ewmh c)
  where c = def { terminal = "urxvt"
                , workspaces = map fst myWorkspaces
                , manageHook = myManageHooks <+> manageSpawn <+> manageHook def
                , layoutHook = myLayoutHook
                , logHook = fadeInactiveLogHook 0xdddddddd <+> logHook def
                , startupHook = myStartupHook <+> startupHook def
                , keys = myKeys
                , focusFollowsMouse = False
                , focusedBorderColor = "red"
                }
