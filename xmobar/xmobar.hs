import Xmobar

-- Example user-defined plugin

data HelloWorld = HelloWorld
    deriving (Read, Show)

instance Exec HelloWorld where
    alias HelloWorld = "hw"
    run   HelloWorld = return "<fc=red>Hello World!!</fc>"

-- Configuration, using predefined monitors as well as our HelloWorld
-- plugin:

config :: Config
config = defaultConfig
  { font = "xft:Fira Code:size=12"
  , additionalFonts = []
  , borderColor = "black"
  , border = NoBorder
  , bgColor = "black"
  , fgColor = "grey"
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
    [ Run $ Network "eth1" ["-L","0","-H","32", "--normal","green","--high","red"] 10
    , Run $ Cpu ["-L","3","-H","50", "--normal","green","--high","red"] 10
    , Run $ Memory ["-t","Mem: <usedratio>%"] 10
    , Run $ Swap [] 10
    , Run $ Com "uname" ["-s","-r"] "" 36000
    , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
    , Run HelloWorld
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%cpu% | %memory% * %swap% | %eth0% - %eth1% }\
               \ %hw% { <fc=#ee9a00>%date%</fc>| %EGPH% | %uname%"
  }

main :: IO ()
main = xmobar config
