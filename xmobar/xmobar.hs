import Xmobar

-- Example user-defined plugin

data HelloWorld = HelloWorld
  deriving (Read, Show)

instance Exec HelloWorld where
  alias HelloWorld = "hw"
  run   HelloWorld = return "<fc=red>Hello World!!</fc>"

data Conky = Conky
  deriving (Read, Show)

main :: IO ()
main = xmobar config

type Color = String

setColorFG :: Color -> String -> String
setColorFG c s = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"

config :: Config
config = defaultConfig
  { font = "xft:Fira Code:size=12"
  , additionalFonts = []
  , border = NoBorder
  , bgColor = black
  , fgColor = white
  , alpha = 255
  , position = BottomW L 100
  , textOffset = -1
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "."
  , allDesktops = True
  , overrideRedirect = True
  , commands =
    [ Run $ Network "enp2s0" ["-L","0","-H","32", "--normal","green","--high","red"] 10
    , Run $ Cpu ["-L","3","-H","50", "--normal","green","--high","red"] 10
    , Run $ Memory ["-t","Mem: <usedratio>%"] 10
    , Run $ Swap [] 10
    , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
    , Run HelloWorld
    ]
  , sepChar = "$"
  , alignSep = "}{"
  , template = "$cpu$ | $memory$ * $swap$ | $eth0$} {" ++ setColorFG yellow "$date$"
  }

black = "#1c1c1c"
black1 = "#626262"

red = "#af005f"
red1 = "#af5f87"

green = "#1c5f5f"
green1 = "#008787"

yellow = "#af871c"
yellow1 = "#dfaf00"

blue = "#1c5f87"
blue1 = "#5f87af"

magenta = "#5f1c5f"
magenta1 = "#875f87"

cyan = "#005f87"
cyan1 = "#0087af"

white = "#afafaf"
white1 = "#e4e4e4"
