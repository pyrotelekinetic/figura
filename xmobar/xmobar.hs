import Xmobar

main :: IO ()
main = xmobar config

type Color = String

setColorFG :: Color -> String -> String
setColorFG c s = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"

seperator :: String
seperator = setColorFG green " | "

pad :: Int -> String
pad 1 = " "
pad n = " " ++ pad (n - 1)

left :: String -> String
left s = s ++ "}"

middle :: String -> String
middle s = s

right :: String -> String
right s = "{" ++ s

config :: Config
config = defaultConfig
  { font = "xft:Fira Code:size=12"
  , additionalFonts = []
  , border = NoBorder
  , bgColor = black
  , fgColor = white
  , alpha = 255 , position = Bottom
  , textOffset = 17
  , iconOffset = 1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "."
  , allDesktops = True
  , overrideRedirect = True
  , commands =
    [ Run $ Cpu ["-L", "6", "-H", "15", "--low", greenBright, "--normal", yellowBright,"--high", redBright] 10
    , Run $ Memory ["-t", "Mem: <usedratio>%"] 10
    , Run $ Swap [] 10
    , Run $ Date "%a, %b %_d, %Y" "date" 43200
    , Run $ Date "%-I:%M %P" "time" 30
    , Run $ MPD ["-t", "<artist> - <title> * <remaining>"] 10
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
    ( left $ concat
      [ pad 1
      , "%mpd%"
      ]
    )
    ++
    ( middle $ concat
      [ setColorFG blueBright "%time%"
      ]
    )
    ++
    ( right $ concat
      [ setColorFG whiteBright "%cpu%"
      , seperator
      , setColorFG blue "%date%"
      , pad 1
      ]
    )
  }

black = "#1c1c1c"
blackBright = "#626262"

red = "#af005f"
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
