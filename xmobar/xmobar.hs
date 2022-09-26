{-# OPTIONS_GHC -threaded #-}
import Xmobar

main :: IO ()
main = xmobar config

type Color = String

setColorFG :: Color -> String -> String
setColorFG c s = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"

seperator :: String
seperator = setColorFG green "|"

pad :: Int -> String
pad 1 = " "
pad n = " " ++ pad (n - 1)

left :: [String] -> String
left ss = concat ss ++ "}"

middle :: [String] -> String
middle ss = concat ss

right :: [String] -> String
right ss = "{" ++ concat ss

config :: Config
config = defaultConfig
	{ font = "xft:Fira Code:size=12:antialias=true, Ricty Diminished:size=12"
	, additionalFonts = []
	, border = NoBorder
	, bgColor = newblack
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
		[ Run $ Cpu
			[ "-S", "True"
			, "-L", "6"
			, "-H", "15"
			, "--low", greenBright
			, "--normal", yellowBright
			, "--high", redBright
			, "-t", "Cpu: <total>"
			] 10
		, Run $ Memory
			[ "-S", "True"
			, "-L", "10"
			, "-H", "16"
			, "--low", greenBright
			, "--normal", yellowBright
			, "--high", redBright
			, "-t", "Mem: <usedratio>"
			] 10
		, Run $ Swap [] 10
		, Run $ Date "%a, %b %-d, %Y" "date" 43200
		, Run $ Date "%-H:%M" "time" 30
		, Run $ MPD ["-M", "25", "-e", "…", "-f", ">", "-b", "=", "-W", "24", "-t", "<artist> - <title> [<statei>][<flags>] <bar>"] 10
		]
	, sepChar = "%"
	, alignSep = "}{"
	, template = myTemplate
	}

myTemplate =
	left
		[ pad 1
		, setColorFG magentaBright "%mpd%"
		]
	++
	middle
		[ setColorFG blueBright "%time%"
		]
	++
	right
		[ setColorFG yellow "%cpu%"
		, pad 1
		, seperator
		, pad 1
		, setColorFG yellow "%memory%"
		, pad 1
		, seperator
		, pad 1
		, setColorFG blueBright "%date%"
		, pad 1
		]

newblack = "#111111"

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
