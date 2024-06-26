{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad ( (<=<) )
import Data.Functor ( (<&>) )
import Graphics.X11.ExtraTypes.XF86
import System.Directory ( getAppUserDataDirectory, doesFileExist, getHomeDirectory )
import System.FilePath.Posix ( (</>) )
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks ( avoidStruts, docks, ToggleStruts(..) )
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.NoBorders ( smartBorders, noBorders )
import XMonad.Layout.SimpleFloat ( simpleFloat )
import XMonad.Util.Run ( spawnPipe, hPutStrLn )
import qualified Data.Map as M
import qualified XMonad.StackSet as W

myWorkspaces :: [(String, KeySym)]
myWorkspaces = [ ("1", xK_1)
               , ("2", xK_2)
               , ("3", xK_parenleft)
               , ("4", xK_dollar)
               , ("5", xK_braceleft)
               , ("6", xK_equal)
               , ("p", xK_p)
               , ("w", xK_w)
               , ("v", xK_v)
               , ("m", xK_m)
               , ("c", xK_c)
               , ("g", xK_g)
               ]

makeWorkspaceKeys :: ButtonMask -> [(String,KeySym)] -> [((ButtonMask,KeySym), X())]
makeWorkspaceKeys mask ws = gotoKeys ws ++ moveKeys ws
    where gotoKeys = map (\(name, key) -> ((mask, key), windows $ W.view name))
          moveKeys = map (\(name, key) -> ((mask .|. shiftMask, key), windows $ W.shift name))

myKeys :: FilePath -> FilePath -> FilePath
       -> XConfig l -> M.Map (ButtonMask, KeySym) (X ())
myKeys _ bin localBin XConfig { terminal = t } = M.fromList $ makeWorkspaceKeys mod4Mask myWorkspaces ++
  [ ((mod1Mask .|. shiftMask, xK_Return), spawn t)
  , ((mod1Mask, xK_p), spawn "dmenu_run")
  , ((mod1Mask, xK_k), spawn "k")
  , ((mod1Mask .|. shiftMask, xK_c), kill)
  , ((mod1Mask, xK_space), sendMessage NextLayout)
  , ((mod1Mask, xK_Tab), windows W.focusDown)
  , ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp )
  , ((mod1Mask, xK_m), windows W.focusMaster )
  , ((mod1Mask, xK_Return), windows W.swapMaster)
  , ((mod1Mask .|. shiftMask, xK_j), windows W.swapDown )
  , ((mod1Mask .|. shiftMask, xK_k), windows W.swapUp )
  , ((mod1Mask, xK_h), sendMessage Shrink)
  , ((mod1Mask, xK_l), sendMessage Expand)
  , ((mod1Mask, xK_t), withFocused $ windows . W.sink)
  , ((mod1Mask, xK_period), spawn $ "pass-pick")
  , ((mod1Mask .|. shiftMask, xK_period), spawn $ "pass-pick -m")
  , ((mod4Mask, xK_b), sendMessage ToggleStruts)
  , ((mod4Mask, xK_comma), sendMessage (IncMasterN 1))
  , ((mod4Mask, xK_period), sendMessage (IncMasterN (-1)))
  , ((mod4Mask, xK_d), spawn "docs")
  , ((0, xF86XK_MonBrightnessUp), spawn $ bin </> "brightness +1")
  , ((0, xF86XK_MonBrightnessDown), spawn $ bin </> "brightness -1")
  , ((shiftMask, xF86XK_MonBrightnessUp), spawn $ bin </> "brightness +5")
  , ((shiftMask, xF86XK_MonBrightnessDown), spawn $ bin </> "brightness -5")
  , ((0, xF86XK_AudioRaiseVolume), spawn $ bin </> "volume +1")
  , ((0, xF86XK_AudioLowerVolume), spawn $ bin </> "volume -1")
  , ((shiftMask, xF86XK_AudioRaiseVolume), spawn $ bin </> "volume +5")
  , ((shiftMask, xF86XK_AudioLowerVolume), spawn $ bin </> "volume -5")
  , ((0, xF86XK_AudioMute), spawn $ bin </> "volume m")
  , ((0, xF86XK_Display), spawn $ localBin </> "displayswitcheroo")
  , ((0, xF86XK_WebCam), spawn $ bin </> "bt")
  , ((0, xK_F9), spawn $ localBin </> "action trigger f9")
  , ((0, xK_F10), spawn $ localBin </> "action trigger f10")
  , ((0, xK_F11), spawn $ localBin </> "action trigger f11")
  , ((0, xF86XK_WLAN), spawn $ bin </> "wifi-fix")
  ]

bars :: XConfig l -> IO (XConfig l)
bars conf = do
  h <- spawnPipe "statusbar run -f"
  return $ docks $ conf { logHook = logHook conf >> dynamicLogWithPP pp { ppOutput = hPutStrLn h } }
    where pp = def { ppCurrent         = dzenColor "#ebac54" "#1B1D1E"
                   , ppVisible         = dzenColor "white" "#1B1D1E"
                   , ppHidden          = dzenColor "white" "#1B1D1E"
                   , ppHiddenNoWindows = dzenColor "#7b7b7b" "#1B1D1E"
                   , ppUrgent          = dzenColor "#ff0000" "#1B1D1E"
                   , ppLayout          = dzenColor "#7b7b7b" "#1B1D1E"
                   , ppWsSep           = " "
                   , ppSep             = " | "
                   , ppTitle           = dzenColor "white" "#1B1D1E" . dzenEscape
                   }

main :: IO ()
main = do
  home <- getHomeDirectory
  let (bin, localBin) = (home </> "bin", home </> ".local" </> "bin")
  bw <- getAppUserDataDirectory "xmonad" <&> (\x -> x </> "border-width") >>= \fn ->
    doesFileExist fn >>= \case
      False -> return 2
      True -> read <$> readFile fn
  xmonad <=< bars $
    def { terminal = "st"
        , workspaces = fst <$> myWorkspaces
        , layoutHook = avoidStruts $ smartBorders (Tall 1 (2/100) (1/2)) ||| noBorders Full ||| simpleFloat
        , startupHook = composeAll [ setWMName "LG3D"
                                   ] <+> startupHook def
        , manageHook = composeAll [ className =? "scidDialog" --> doFloat
                                  ] <+> manageHook def
        , keys = myKeys home bin localBin
        , focusFollowsMouse = False
        , focusedBorderColor = "red"
        , borderWidth = bw
        }
