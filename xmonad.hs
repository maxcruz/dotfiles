-------------------------------------------------------------------------------
--                  __  ____  __                       _                     --
--                  \ \/ /  \/  | ___  _ __   __ _  __| |                    --
--                   \  /| |\/| |/ _ \| '_ \ / _` |/ _` |                    --
--                   /  \| |  | | (_) | | | | (_| | (_| |                    --
--                  /_/\_\_|  |_|\___/|_| |_|\__,_|\__,_|                    --
--                                                                           --
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Import                                                                    --
-------------------------------------------------------------------------------

-- Xmonad, a minimalist, tiling window manager for X11
import XMonad 

-- Xmonad calls the logHook with every internal state update (useful for xmobar)
import XMonad.Hooks.DynamicLog

-- Provides tools to automatically manage dock type programs (like xmobar)
import XMonad.Hooks.ManageDocks

-- Lets you configure an action to occur when a window demands your attention
import XMonad.Hooks.UrgencyHook 

-- Helper functions to be used in manageHook.
import XMonad.Hooks.ManageHelpers

-- Add a configurable amount of space around windows
import XMonad.Layout.Spacing

-- Provides several commands to run an external process (spawnPipe xmobar)
import XMonad.Util.Run

-- Spawning a command once, and only once
import XMonad.Util.SpawnOnce

-- Parsing keybindings specified in a special (emacs-like) format (additionalKeys)
import XMonad.Util.EZConfig

-- Setting the default mouse cursor
import XMonad.Util.Cursor

-- Associate the X titles of windows with them (notify-osd)
import XMonad.Util.NamedWindows

-- Data type that encodes a window manager abstraction (notify-osd)
import qualified XMonad.StackSet as W

-- The standard IO library
import System.IO

-------------------------------------------------------------------------------
-- Variables                                                                 --
-------------------------------------------------------------------------------

myWorkspaces = [ "α", "β", "γ", "δ", "ε" ]
myTerminal = "termite"
myTitleColor = "#8CDBD8"
myNormalBorderColor = "#627484"
myFocusedBorderColor = "#AFE0A1"
myFocusFollowsMouse = True
myClickJustFocuses = True
myBorderWidth = 1
myWindowSpace = 5

-------------------------------------------------------------------------------
-- Panel                                                                     --
-------------------------------------------------------------------------------

-- Panel command
myPanel = "xmobar" 

-- Format for xmobar
myPanelFormat = xmobarPP 
    { ppTitle = xmobarColor myTitleColor "" . shorten 50 }

-- Log output to panel
myLogHook panel = dynamicLogWithPP $ myPanelFormat
    { ppOutput = hPutStrLn panel }

-------------------------------------------------------------------------------
-- Hooks                                                                     --
-------------------------------------------------------------------------------

-- Send notify OSD messages using UrgencyHook
data NotifyUrgencyHook = NotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook NotifyUrgencyHook where
    urgencyHook NotifyUrgencyHook window = do
        name     <- getName window
        Just idx <- fmap (W.findTag window) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ idx]

-- Special actions to be performed on newly created windows
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , isDialog --> doCenterFloat
    , className =? "Gimp" --> doFloat
    , manageDocks
    ]

myLayoutHook = avoidStruts $ spacing myWindowSpace $ Tall 1 (3/100) (1/2)

-------------------------------------------------------------------------------
-- Shortcuts                                                                 --
-------------------------------------------------------------------------------

myKeysCombination = 
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    ]

-------------------------------------------------------------------------------
-- Startup                                                                   --
-------------------------------------------------------------------------------

startUp :: X()
startUp = do
	spawnOnce "compton"

-------------------------------------------------------------------------------
-- Main                                                                      --
-------------------------------------------------------------------------------

main :: IO () 
main = do
    panel <- spawnPipe myPanel
    xmonad 
        $ withUrgencyHook NotifyUrgencyHook
        $ defaultConfig
        { modMask = mod4Mask
        , terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        , clickJustFocuses = myClickJustFocuses
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor 
        , startupHook = setDefaultCursor xC_left_ptr
        , handleEventHook = docksEventHook <+> handleEventHook defaultConfig
        , manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , logHook = myLogHook panel 
        }  `additionalKeys` myKeysCombination
