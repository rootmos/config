-- Import statements
import Dzen
import XMonad
import Control.Monad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.EwmhDesktops

myTerminal = "urxvt"

myWorkspaces =
    [
        ("1", xK_1),
        ("2", xK_2),
        ("web", xK_w),
        ("mail", xK_m),
        ("pdf", xK_p),
        ("chat", xK_c),
        ("graveyard", xK_g)
    ]

makeWorkspaceKeys :: ButtonMask -> [(String,KeySym)] -> [((ButtonMask,KeySym), X())]
makeWorkspaceKeys mask =
    map ( \(name, key) -> ((mask, key), windows $ W.greedyView name) )
    

myManageHook = composeAll . concat $
    [
        [className =? b --> viewShift "web" | b <- myClassWebShifts],
        [className =? b --> viewShift "mail" | b <- myClassMailShifts],
        [className =? b --> viewShift "chat" | b <- myClassChatShifts]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassWebShifts = ["Chrome"]
        myClassMailShifts = ["Thunderbird"]
        myClassChatShifts = ["Pidgin"]

keysToAdd x =
    makeWorkspaceKeys mod4Mask myWorkspaces
    ++ []

keysToRemove x =
    [
    ]

strippedKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))

 
-- Workspace dzen bar
myStatusBar = DzenConf {
      x_position = Just 0
    , y_position = Just 0
    , width      = Just 1920
    , height     = Just 13
    , alignment  = Just LeftAlign
    , font       = Just "Bitstream Sans Vera:pixelsize=10"
    , fg_color   = Just "#ffffff"
    , bg_color   = Just "#000000"
    , exec       = []
    , addargs    = []
}
 
-- Pretty printer for dzen workspace bar
myPrettyPrinter h = dzenPP {
      ppOutput          = hPutStrLn h
    , ppCurrent         = dzenColor "#000000" "#e5e5e5"
    , ppHidden          = dzenColor "#e5e5e5" "#000000"
    , ppHiddenNoWindows = dzenColor "#444444" "#000000"
    , ppUrgent          = dzenColor "#ff0000" "#000000". dzenStrip
    , ppWsSep           = " "
    , ppSep             = " | "
}

myLogHook = logHook defaultConfig <+> ewmhDesktopsLogHook
myDzenLogHook h = dynamicLogWithPP $ myPrettyPrinter h

myStartupHook config = do
   startupHook config   
   spawn "xreaddb"
   spawn "start-xsettingsd"

myLayoutHook config = avoidStruts $ layoutHook config

 
main = do
    workspaceBar <- spawnDzen myStatusBar
    xmonad $ defaultConfig
        {
            terminal = myTerminal,
            workspaces = map fst myWorkspaces,
            manageHook = myManageHook <+> manageDocks,
            keys = myKeys,
            focusFollowsMouse = False,
            logHook = myLogHook <+> myDzenLogHook workspaceBar,
            startupHook = myStartupHook defaultConfig,
            layoutHook = myLayoutHook defaultConfig
        }
 


