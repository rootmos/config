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
        ("chat", xK_c),
        ("graveyard", xK_g)
    ]

makeWorkspaceKeys :: ButtonMask -> [(String,KeySym)] -> [((ButtonMask,KeySym), X())]
makeWorkspaceKeys mask workspaces = gotoKeys workspaces ++ moveKeys workspaces
    where
        gotoKeys = map (\(name, key) -> ((mask, key), windows $ W.greedyView name))
        moveKeys = map (\(name, key) -> ((mask .|. shiftMask, key), windows $ W.shift name))
    

viewShift = doF . liftM2 (.) W.greedyView W.shift

myWebShifts = [className =? b --> viewShift "web" | b <- ["chromium-browser"]]
myChatShifts = [className =? b --> viewShift "chat" | b <- ["Slack"]]
myMusicShifts = [className =? b --> viewShift "music" | b <- ["Spotify"]]
myFloats = [className =? "mplayer2" --> doFloat,
            className =? "VirtualBox" --> doFloat]
myManageHook = manageDocks <+> (composeAll . concat $ [myFloats, myWebShifts, myMusicShifts, myChatShifts])

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
       , ((mod4Mask, xK_v), spawn "display.py --verbose --syslog --choose")
       , ((mod4Mask, xK_l), spawn "lock")
       , ((0, xK_Print), spawn "dv")
       , ((0, xK_Scroll_Lock), spawn "se")
       , ((0, xK_Pause), spawn "us")
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

myXmonadBar = "~/bin/dzen2-resizing -l 50"
myStatusBar = "conky -c ~/.xmonad/conky_dzen | ~/bin/dzen2-resizing -r 50"
 
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
