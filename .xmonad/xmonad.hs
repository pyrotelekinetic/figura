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
import StatusBar
import System.IO
import Graphics.X11.ExtraTypes.XF86
import Data.Map (fromList)

main = do
  xmproc <- myBar ""
  xmonad $ ewmh def
    { terminal = "qterminal"
    , modMask = mod4Mask
    , XMonad.borderWidth = 1
    , XMonad.workspaces = withScreens 2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , manageHook = manageDocks <+> manageHook def
    , layoutHook = avoidStruts $ layoutHook def
    , keys = myKeys
    , handleEventHook = handleEventHook def <+> docksEventHook
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = myBar
      ,  ppTitle = xmobarColor "" "" . shorten 20
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


--import Xmobar

-- Example user-defined plugin

--data HelloWorld = HelloWorld
--  deriving (Read, Show)
--
--instance Exec HelloWorld where
--  alias HelloWorld = "hw"
--  run   HelloWorld = return "<fc=red>Hello World!!</fc>"
--
--data Conky = Conky
--  deriving (Read, Show)
--
--bar :: IO ()
--bar = Xmobar.xmobar xmobarConfig
--
----type Color = String
--
--setColorFG :: String -> String -> String
--setColorFG c s = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"
--
--xmobarConfig :: Config
--xmobarConfig = Xmobar.defaultConfig
--  { font = "xft:Fira Code:size=12"
--  , additionalFonts = []
--  , border = NoBorder
--  , bgColor = black
--  , fgColor = white
--  , alpha = 255
--  , position = BottomW Xmobar.L 100
--  , textOffset = -1
--  , iconOffset = -1
--  , lowerOnStart = True
--  , pickBroadest = False
--  , persistent = False
--  , hideOnStart = False
--  , iconRoot = "."
--  , allDesktops = True
--  , overrideRedirect = True
--  , commands =
--    [ Run $ Network "enp2s0" ["-L","0","-H","32", "--normal","green","--high","red"] 10
--    , Run $ Cpu ["-L","3","-H","50", "--normal","green","--high","red"] 10
--    , Run $ Memory ["-t","Mem: <usedratio>%"] 10
--    , Run $ Swap [] 10
--    , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
--    , Run HelloWorld
--    ]
--  , sepChar = "$"
--  , alignSep = "}{"
--  , template = "$cpu$ | $memory$ * $swap$ | $eth0$} {" ++ setColorFG yellow "$date$"
--  }
--
--black = "#1c1c1c"
--black1 = "#626262"
--
--red = "#af005f"
--red1 = "#af5f87"
--
--green = "#1c5f5f"
--green1 = "#008787"
--
--yellow = "#af871c"
--yellow1 = "#dfaf00"
--
--blue = "#1c5f87"
--blue1 = "#5f87af"
--
--magenta = "#5f1c5f"
--magenta1 = "#875f87"
--
--cyan = "#005f87"
--cyan1 = "#0087af"
--
--white = "#afafaf"
--white1 = "#e4e4e4"
