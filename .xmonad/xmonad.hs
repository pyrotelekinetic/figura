import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.StackSet
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS as CWS
import XMonad.Actions.DynamicWorkspaceOrder as DWO
import System.IO
import Graphics.X11.ExtraTypes.XF86
import Data.Map (fromList)

main = do
  xmproc <- spawnPipe myStatusBar
  xmonad $ ewmh defaultConfig
    { terminal = "qterminal"
    , modMask = mod4Mask
    , borderWidth = 1
    , XMonad.workspaces = withScreens 2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , keys = myKeys
    , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "" "" . shorten 20
      , ppHiddenNoWindows = xmobarColor "" ""
      }
    }

myStatusBar = "xmobar ~/dotfiles/xmobar/xmobar.hs"
--myStatusBar = "conky -c ~/.conky_dzen | dzen2 -xs 1 -y 1179"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = fromList $
  [ ((modMask, xK_Return), spawn $ terminal conf)
  , ((modMask, xK_semicolon), spawn "dmenu_run") 
  , ((modMask, xK_q), spawn "xmonad --restart")
  , ((modMask, xK_x), kill)
  ]
  ++
  [ ((modMask, xK_j), windows focusDown)
  , ((modMask, xK_k), windows focusUp)
  , ((modMask, xK_Down), windows focusDown)
  , ((modMask, xK_Up), windows focusUp)
  , ((modMask .|. shiftMask, xK_j), windows swapDown)
  , ((modMask .|. shiftMask, xK_k), windows swapUp)
  , ((modMask, xK_h), sendMessage Shrink)
  , ((modMask, xK_l), sendMessage Expand)
  , ((modMask, xK_comma), sendMessage (IncMasterN 1))
  , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
  , ((modMask, xK_space), sendMessage NextLayout)
  , ((modMask, xK_z), windows swapMaster)
  , ((modMask, xK_t), withFocused $ windows . sink)
  , ((modMask, xK_Tab), nextScreen)
  , ((modMask .|. shiftMask, xK_Tab), CWS.moveTo Prev NonEmptyWS)
  , ((modMask, xK_grave), DWO.moveTo Next HiddenNonEmptyWS)
  ]
  ++
  [ ((0, xF86XK_Tools), spawn "mpd")
  , ((0, xF86XK_AudioPlay), spawn "mpc toggle")
  , ((0, xF86XK_AudioPrev), spawn "mpc prev")
  , ((0, xF86XK_AudioNext), spawn "mpc next")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 2%+")
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 2%-")
  , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
  ]
  ++
  [ ((m .|. mod4Mask, k), windows $ onCurrentScreen f i) 
    | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
    , (f, m) <- [(greedyView, 0), (shift, shiftMask)]
  ]
