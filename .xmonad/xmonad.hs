import XMonad
import Control.Monad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.SetWMName
import Graphics.X11.ExtraTypes.XF86 

import System.IO
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat

myTerminal = "urxvt"

myWorkspaces =
    [
        ("1", xK_1),
        ("2", xK_2),
        ("3", xK_parenleft),
        ("4", xK_dollar),
        ("web", xK_w),
        ("music", xK_m),
        ("video", xK_v),
        ("netflixy+vods", xK_n),
        ("twitch", xK_t),
        ("chat", xK_c),
        ("graveyard", xK_g)
    ]

makeWorkspaceKeys :: ButtonMask -> [(String,KeySym)] -> [((ButtonMask,KeySym), X())]
makeWorkspaceKeys mask workspaces = gotoKeys workspaces ++ moveKeys workspaces
    where
        gotoKeys = map (\(name, key) -> ((mask, key), windows $ W.greedyView name))
        moveKeys = map (\(name, key) -> ((mask .|. shiftMask, key), windows $ W.shift name))
    

viewShift = doF . liftM2 (.) W.greedyView W.shift

myWebShifts = [className =? b --> viewShift "web" | b <- ["Chromium-browser"]]
myMusicShifts = [className =? b --> viewShift "music" | b <- ["Spotify"]]
myTwitchShifts = [title =? b --> viewShift "twitch" | b <- ["Livestreamer Twitch GUI"]]
myFloats = [className =? "mplayer2" --> doFloat,
            className =? "VirtualBox" --> doFloat]
myManageHook = manageDocks <+> (composeAll . concat $ [myFloats, myWebShifts, myMusicShifts, myTwitchShifts])

myLayoutHook = avoidStruts $ myTall ||| myFull ||| simpleFloat
    where
        myTall = smartBorders $ Tall 1 (2/100) (1/2)
        myFull = noBorders $ Full

keysToAdd x =
    makeWorkspaceKeys mod4Mask myWorkspaces
    ++ [ ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10")
       , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10")
       , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 5%+")
       , ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 5%-")
       , ((0, xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle")
       , ((0, xF86XK_Display), spawn "switch-display")
       ]

keysToRemove x =
    [
    ]

strippedKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
    , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
    , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
    , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
    , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
    , ppWsSep             =   " "
    , ppSep               =   "  |  "
    , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
    , ppOutput            =   hPutStrLn h
    }

myXmonadBar = "dzen2 -x '0' -y '0' -w '1620' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E' -fn '-*-*-*-*-*-*-10-*-*-*-*-*-*-*'"
myStatusBar = "conky -c ~/.xmonad/conky_dzen_desktop | dzen2 -x '1620' -y '0' -w '300' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -fn '-*-*-*-*-*-*-10-*-*-*-*-*-*-*'"
 
main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar

    xmonad $ defaultConfig
        {
            terminal = myTerminal,
            workspaces = map fst myWorkspaces,
            manageHook = myManageHook,
            layoutHook = myLayoutHook,
            keys = myKeys,
            focusFollowsMouse = False,
            logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd,
            focusedBorderColor = "grey",
            startupHook = setWMName "LG3D"
        }
