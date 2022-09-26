{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE NamedFieldPuns #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders (noBorders)
import XMonad.StackSet
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen, hiddenWS, emptyWS, WSType (..))
import XMonad.Actions.DynamicWorkspaceOrder (moveTo)
import XMonad.Actions.SpawnOn (manageSpawn, spawnHere, spawnAndDo)

import Graphics.X11.ExtraTypes.XF86
import Data.Map (fromList)

main = do
	spawn "xbanish"
	spawn "killall mpd ; mpd"
	spawn "killall mpdscribble ; mpdscribble"
	xmproc <- spawnPipe myStatusBar
	xmonad $ ewmh $ docks $ def
		{ terminal = "qterminal"
		, modMask = mod4Mask
		, XMonad.borderWidth = 1
		, XMonad.focusedBorderColor = magenta
		, XMonad.normalBorderColor = blackBright
		, XMonad.workspaces = withScreens 2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
		, manageHook = manageDocks <+> manageSpawn <+> manageHook def
		, layoutHook = avoidStruts myLayout
		, keys = myKeys
		, handleEventHook = handleEventHook def <+> docksEventHook
		, logHook = dynamicLogWithPP xmobarPP
--			{ ppOutput = hPutStrLn xmproc
--			,  ppTitle = xmobarColor "" "" . shorten 20
--			, ppHiddenNoWindows = xmobarColor "" ""
--			}
		}

myStatusBar = "xmobar ~/dotfiles/xmobar/xmobar.hs"

myLayout = long ||| tall ||| noBorders Full
	where
	tall = Tall 1 (2/100) (1/2)
	long = Mirror tall

myKeys conf@(XConfig {modMask}) = fromList $
	[ ((modMask, xK_j), windows focusDown)
	, ((modMask, xK_k), windows focusUp)
	, ((modMask .|. shiftMask, xK_j), windows swapDown)
	, ((modMask .|. shiftMask, xK_k), windows swapUp)
	, ((modMask, xK_h), sendMessage Shrink)
	, ((modMask, xK_l), sendMessage Expand)
	, ((modMask, xK_comma), sendMessage (IncMasterN 1))
	, ((modMask, xK_period), sendMessage (IncMasterN (-1)))
	, ((modMask .|. shiftMask, xK_space), setLayout $ layoutHook conf)
	, ((modMask, xK_space), sendMessage NextLayout)
	, ((modMask, xK_z), windows swapMaster)
	, ((modMask, xK_t), withFocused $ windows . sink)
	, ((modMask, xK_Tab), nextScreen)
	, ((modMask .|. shiftMask, xK_Tab), shiftNextScreen >> nextScreen)
	, ((modMask, xK_grave), moveTo Next (hiddenWS :&: Not emptyWS))
	, ((modMask, xK_f), sendMessage ToggleStruts)
	]
	++
	[ ((modMask, xK_Return), spawn $ terminal conf)
	, ((modMask, xK_semicolon), spawnHere "dmenu_run")
	, ((modMask, xK_q), spawn "xmonad --restart && killall xmobar")
	, ((modMask .|. shiftMask, xK_q), spawn "killall xmobar && xmobar -r ~/dotfiles/xmobar/xmobar.hs")
	, ((modMask, xK_x), kill)
	, ((modMask, xK_Scroll_Lock), spawn "~/.screenlayout/toggle-layout.sh")
	]
	++
	[ ((0, xK_Home), spawnHere "qterminal -e ncmpcpp")
	, ((0, xK_Scroll_Lock), spawn "mpc toggle")
	, ((0, xK_Print), spawn "mpc prev")
	, ((0, xK_Pause), spawn "mpc next")
	, ((modMask, xK_Up), spawn "amixer -q sset Master 2%+")
	, ((modMask, xK_Down), spawn "amixer -q sset Master 2%-")
	, ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 2%+")
	, ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 2%-")
	, ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
	]
	++
	[ ((modMask, xK_d), spawnHere "discord")
	, ((modMask, xK_b), spawnHere "firefox")
	, ((modMask, xK_s), spawnHere "steam")
	]
	++
	[ ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
		| (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
		, (f, m) <- [(greedyView, 0), (shift, shiftMask)]
	]
	++
	[ ((modMask, xK_bracketleft), spawn "dunstctl close")
	, ((modMask, xK_bracketright), spawn "dunstctl history-pop")
	]


black = "#1c1c1c"
blackBright = "#626262"

red = "#af004f"
redBright = "#af5f87"

green = "#1c5f5f"
greenBright = "#008787"

yellow = "#af871c"
yellowBright = "#dfaf00"

blue = "#1c5f87"
blueBright = "#5f87af"

magenta = "#5f1c5f"
magentaBright = "#875f87"

cyan = "#005f87"
cyanBright = "#0087af"

white = "#afafaf"
whiteBright = "#e4e4e4"
