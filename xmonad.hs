
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myWorkspaces = [ "WEB", "CODE", "SHELL", "CHAT", "MEDIA", "OTHER" ]
myTerminal = "termite"
myTitleColor = "#8CDBD8"
myNormalBorderColor = "#627484"
myFocusedBorderColor = "#AFE0A1"
myFocusFollowsMouse = True
myClickJustFocuses = True
myBorderWidth = 5

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , title =? "glxgears" --> doShift (myWorkspaces !! 2)
    , className =? "Surf" --> doShift "WEB"
    , className =? "Gimp" --> doFloat
    , manageDocks
    ]
 
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
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

