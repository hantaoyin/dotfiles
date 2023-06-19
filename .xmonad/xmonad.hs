--
-- xmonad config file.
--
-- Based on a template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Plane
import XMonad.Actions.WindowMenu
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Util.EZConfig

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import Data.Monoid
import System.Exit
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces =
    [ "1:term", "2:web", "3:pdf", "4:pdf", "5"
    , "6", "7", "8", "9:gimp", "0"
    , "F1", "F2", "F3", "F4", "F5"
    , "F6", "F7", "F8", "F9", "F0" ]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- launch a terminal
    , ((modm              , xK_p     ), spawn "dmenu_run -i -fn 6x13") -- launch dmenu
    , ((modm              , xK_F12   ), spawn "xscreensaver-command -lock") -- lock the screen

    , ((modm .|. shiftMask, xK_c     ), kill) -- close focused window
    , ((modm,               xK_space ), sendMessage NextLayout) -- Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) --  Reset the layouts on the current workspace to default
    , ((modm,               xK_n     ), refresh) -- Resize viewed windows to the correct size
    , ((modm,               xK_Tab   ), windows W.focusDown) -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown) -- Move focus to the next window
    , ((modm,               xK_k     ), windows W.focusUp  ) -- Move focus to the previous window
    , ((modm,               xK_m     ), windows W.focusMaster  ) -- Move focus to the master window 
    , ((modm,               xK_Return), windows W.swapMaster) -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  ) -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    ) -- Swap the focused window with the previous window
    , ((modm,               xK_h     ), sendMessage Shrink) -- Shrink the master area
    , ((modm,               xK_l     ), sendMessage Expand) -- Expand the master area
    , ((modm,               xK_t     ), withFocused $ windows . W.sink) -- Push window back into tiling
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1)) -- Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1))) -- Decrement the number of windows in the master area

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- Quit xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart") ] -- Restart xmonad
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ]
    ++

    [ ((modm              , xK_0      ), windows $ W.greedyView "0:rhy")
    , ((modm .|. shiftMask, xK_0      ), windows $ W.shift "0:rhy")
    , ((modm              , xK_F1     ), windows $ W.greedyView "F1")
    , ((modm .|. shiftMask, xK_F1     ), windows $ W.shift "F1")
    , ((modm              , xK_F2     ), windows $ W.greedyView "F2")
    , ((modm .|. shiftMask, xK_F2     ), windows $ W.shift "F2")
    , ((modm              , xK_F3     ), windows $ W.greedyView "F3")
    , ((modm .|. shiftMask, xK_F3     ), windows $ W.shift "F3")
    , ((modm              , xK_F4     ), windows $ W.greedyView "F4")
    , ((modm .|. shiftMask, xK_F4     ), windows $ W.shift "F4")
    , ((modm              , xK_F5     ), windows $ W.greedyView "F5")
    , ((modm .|. shiftMask, xK_F5     ), windows $ W.shift "F5")
    , ((modm              , xK_F6     ), windows $ W.greedyView "F6")
    , ((modm .|. shiftMask, xK_F6     ), windows $ W.shift "F6")
    , ((modm              , xK_F7     ), windows $ W.greedyView "F7")
    , ((modm .|. shiftMask, xK_F7     ), windows $ W.shift "F7")
    , ((modm              , xK_F8     ), windows $ W.greedyView "F8")
    , ((modm .|. shiftMask, xK_F8     ), windows $ W.shift "F8")
    , ((modm              , xK_F9     ), windows $ W.greedyView "F9")
    , ((modm .|. shiftMask, xK_F9     ), windows $ W.shift "F9")
    , ((modm              , xK_F10    ), windows $ W.greedyView "F0")
    , ((modm .|. shiftMask, xK_F10    ), windows $ W.shift "F0") ]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] ]
    ++

    [ ((modm .|. shiftMask, xK_m      ), windowMenu)
--    , ((modm              , xK_a      ), goToSelected gsConfig)
--    , ((modm              , xK_s      ), bringSelected gsConfig)
    ]
    ++

    -- mod-v, Make focused window always visible
    -- mod-shift-v, Toggle window state back
    --
    [ ((modm              , xK_v ), windows copyToAll)
    , ((modm .|. shiftMask, xK_v ), killAllOtherCopies) ]
    ++

    [ ((modm              , xK_quoteleft), toggleWS) ]
    ++

    (M.toList $ planeKeys modm (Lines 2) Finite)
 ++
    
    [ ((modm              , xK_Up   ), spawn "amixer set Master 3+" )
    , ((modm              , xK_Down ), spawn "amixer set Master 3-" ) ]


-- gsConfig = defaultGSConfig
--    { gs_font = "xft:Dejavu Sans Mono-9"
--    , gs_cellheight = 30
--    , gs_cellwidth = 300
--    , gs_cellpadding = 10 }

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ onWorkspace "9:gimp" gimp $ tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

     gimp = withIM 0.11 (Role "gimp-toolbox") $
            reflectHoriz $
            withIM 0.15 (Role "gimp-dock") Full

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = (manageDocks <+>) $ composeAll $
    [ className =? "MPlayer"        --> doFloat
    , title =? "qiv"                --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
    ++
    [ className =? "Gimp"           --> doShift "9:gimp"
    , title =? "Gnuplot"            --> doFloat
    , isFullscreen                  --> doFullFloat ]

------------------------------------------------------------------------
-- Event handling

-- EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
--    spawn "xmobar -x 1"
--    spawn "killall conky ; conky"
--    spawn "dropbox start"
--    spawn "killall ibus-daemon ; ibus-daemon"
--    spawn "sleep 3 ; killall stardict ; stardict"
--    spawn "sleep 1 ; killall tomboy ; tomboy"
--    spawn "sleep 5 ; killall pidgin; pidgin"
--    spawn "nm-applet"
--    spawn "gnome-settings-daemon"
    return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
-- main = do
--  xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc"
--  xmonad $ defaults {
--    logHook = dynamicLogWithPP xmobarPP {
--        ppOutput = hPutStrLn xmproc
--      , ppTitle = xmobarColor "green" "" . shorten 100
--    }
--  }
main = do
  xmonad $ defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
defaults = ewmh $ def {
  -- The preferred terminal program, which is used in a binding below and by
  -- certain contrib modules.
  terminal           = "urxvt",

  -- Whether focus follows the mouse pointer.
  focusFollowsMouse  = True,
    
  -- Width of the window border in pixels.
  borderWidth        = 0,

  -- modMask lets you specify which modkey you want to use. The default
  -- is mod1Mask ("left alt").  You may also consider using mod3Mask
  -- ("right alt"), which does not conflict with emacs keybindings. The
  -- "windows key" is usually mod4Mask.
  modMask            = mod4Mask,
  workspaces         = myWorkspaces,

  -- Border colors for unfocused and focused windows, respectively.
  normalBorderColor  = "#dddddd",
  focusedBorderColor = "#ff0000",

  -- key bindings
  keys               = myKeys,
  mouseBindings      = myMouseBindings,

  -- hooks, layouts
  layoutHook         = myLayout,
  manageHook         = myManageHook,
  handleEventHook    = myEventHook,
  logHook            = myLogHook,
  startupHook        = myStartupHook
}
