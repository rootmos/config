import XMonad
import Control.Monad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Hooks.ICCCMFocus

import System.IO
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive

myTerminal = "urxvt"

myWorkspaces =
    [
        ("1", xK_1),
        ("2", xK_2),
        ("web", xK_w),
        ("music", xK_m),
        ("pdf", xK_p),
        ("chat", xK_c),
        ("graveyard", xK_g)
    ]

makeWorkspaceKeys :: ButtonMask -> [(String,KeySym)] -> [((ButtonMask,KeySym), X())]
makeWorkspaceKeys mask workspaces = gotoKeys workspaces ++ moveKeys workspaces
    where
        gotoKeys = map (\(name, key) -> ((mask, key), windows $ W.greedyView name))
        moveKeys = map (\(name, key) -> ((mask .|. shiftMask, key), windows $ W.shift name))
    

myShifts = composeAll . concat $
    [
        [className =? b --> viewShift "web" | b <- myClassWebShifts  ]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassWebShifts = ["Chromium-browser"]
myManageHook = manageDocks <+> myShifts

myLayoutHook = avoidStruts $ Tall 1 (2/100) (1/2) ||| Full

keysToAdd x =
    makeWorkspaceKeys mod4Mask myWorkspaces
    ++ []

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

myXmonadBar = "dzen2 -x '0' -y '0' -h '24' -w '1960' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
myStatusBar = "conky -c ~/.xmonad/conky_dzen | dzen2 -x '1960' -y '0' -w '600' -h '24' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF'"
 
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
            logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
        }
