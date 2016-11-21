
import XMonad
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import qualified XMonad.StackSet as W

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ idx]

myWorkspaces = [ "CODE", "WEB", "SHELL", "CHAT", "MEDIA", "OTHER" ]
myTerminal = "termite"
myTitleColor = "#8CDBD8"
myNormalBorderColor = "#627484"
myFocusedBorderColor = "#AFE0A1"
myFocusFollowsMouse = True
myClickJustFocuses = True
myBorderWidth = 5

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Surf" --> doShift "WEB"
    , className =? "Gimp" --> doFloat
    , className =? "Galculator" --> doCenterFloat
    , manageDocks
    ]

main :: IO () 
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad 
        $ withUrgencyHook LibNotifyUrgencyHook
        $ defaultConfig
        { modMask = mod4Mask
        , terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        , clickJustFocuses = myClickJustFocuses
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor 
        , handleEventHook = docksEventHook <+> handleEventHook defaultConfig
        , manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor myTitleColor "" . shorten 50
                        }
        }  `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]

