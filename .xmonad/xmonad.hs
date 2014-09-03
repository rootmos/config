import XMonad
import Control.Monad
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
        [className =? b --> viewShift "web" | b <- myClassWebShifts  ]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassWebShifts = ["Chromium-browser"]

keysToAdd x =
    makeWorkspaceKeys mod4Mask myWorkspaces
    ++ []

keysToRemove x =
    [
    ]

strippedKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))

myLogHook = logHook defaultConfig <+> ewmhDesktopsLogHook
 
main = do
    xmonad $ defaultConfig
        {
            terminal = myTerminal,
            workspaces = map fst myWorkspaces,
            manageHook = myManageHook,
            keys = myKeys,
            focusFollowsMouse = False,
            logHook = myLogHook
        }
