{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Maybe
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import System.IO.Error
import System.Environment ( lookupEnv )
import System.Directory ( getAppUserDataDirectory, doesFileExist )
import Text.Printf
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Util.Run
import XMonad.Util.Run
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import Utils

myWorkspaces =
    [
        ("1", xK_1),
        ("2", xK_2),
        ("3", xK_parenleft),
        ("4", xK_dollar),
        ("5", xK_braceleft),
        ("6", xK_equal),
        ("p", xK_p),
        ("w", xK_w),
        ("v", xK_v),
        ("m", xK_m),
        ("c", xK_c),
        ("g", xK_g)
    ]

viewShift = doF . liftM2 (.) W.greedyView W.shift

myShifts = [ className =? "Chromium" --> doShift "w"
           , className =? "Spotify" --> doShift "m"
           , className =? "Zathura" --> viewShift "p"
           , className =? "Kodi" --> viewShift "v"
           ]
myFloats = [ className =? "MPlayer" --> doFloat
           , className =? "VirtualBox" --> doFloat
           ]
myManageHooks = composeAll $ myShifts ++ myFloats

myLayoutHook = myTall ||| myFull ||| simpleFloat
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
       {-, ((mod4Mask, xK_n), swapNextScreen)-}
       {-, ((mod4Mask, xK_t), nextScreen)-}
       , ((modMask x, xK_d), spawn "docs")
       , ((modMask x, xK_w), spawn "/home/gustav/bin/netctl-switch-to-menu")
       , ((modMask x, xK_period), spawn "/home/gustav/bin/pass-pick")
       , ((0, xF86XK_WLAN), spawn "sudo /home/gustav/bin/wifi-fix")
       , ((mod4Mask, xK_comma), sendMessage (IncMasterN 1))
       , ((mod4Mask, xK_period), sendMessage (IncMasterN (-1)))
       ]

keysToRemove x =
    [ (modMask x, xK_comma)
    , (modMask x, xK_period)
    ]

strippedKeys x = foldr M.delete (keys def x) (keysToRemove x)
myKeys x = M.union (M.fromList (keysToAdd x)) (strippedKeys x)

myDzenPP = def
  { ppCurrent           =   dzenColor "#ebac54" "#1B1D1E"
  , ppVisible           =   dzenColor "white" "#1B1D1E"
  , ppHidden            =   dzenColor "white" "#1B1D1E"
  , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E"
  , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E"
  , ppLayout            =   dzenColor "#7b7b7b" "#1B1D1E"
  , ppWsSep             =   " "
  , ppSep               =   " | "
  , ppTitle             =   dzenColor "white" "#1B1D1E" . dzenEscape
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

currentWidth :: IO Int
currentWidth = do
  (root, res) <- currentDefaultDisplay
  setup <- fetchSetup root res
  case filter isOutputEnabled . M.elems $ setupOutputs setup of
    Output { outputMonitor = Just mid } : _ -> do
      let Just monitor = lookupMonitor mid setup
      return $ monitorWidth monitor
    _ -> return 1910

bars conf = do
  width <- currentWidth
  (statusBar' (myStatusBar width) myDzenPP toggleStrutsKey conf)
    >>= (statusBar (myXmonadBar width) myDzenPP toggleStrutsKey)
    where right = 900
          left width = width - right
          font = "-*-helvetica-*-r-*-*-14-*-*-*-*-*-*-*" :: String
          dzen = printf "dzen2 -e 'onstart=lower' -dock -fg '#FFFFFF' -bg '#1B1D1E' -fn '%s'" font :: String
          myXmonadBar width = printf "%s -x '0' -y '0' -w '%d' -ta 'l'" dzen (left width)
          myStatusBar width = printf "conky -c ~/.xmonad/conky_dzen 0>/dev/null | %s -x '%d' -y '0' -w '%d' -ta 'r'" dzen (left width) right

myStartupHook = composeAll [ setWMName "LG3D" ]

main :: IO ()
main = do
  fn <- getAppUserDataDirectory "xmonad/border-width"
  bw <- doesFileExist fn >>= \case
    False -> return 4
    True -> read <$> readFile fn
  xmonad =<< bars (ewmh $ c bw)
    where c bw = def { terminal = "term"
                  , workspaces = fst <$> myWorkspaces
                  , manageHook = myManageHooks <+> manageSpawn <+> manageHook def
                  , layoutHook = myLayoutHook
                  , logHook = fadeInactiveLogHook 0xdddddddd <+> logHook def
                  , startupHook = myStartupHook <+> startupHook def
                  , keys = myKeys
                  , focusFollowsMouse = False
                  , focusedBorderColor = "red"
                  , borderWidth = bw
                  }
