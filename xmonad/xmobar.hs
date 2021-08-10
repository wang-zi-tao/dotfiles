-- This file is generated from "README.org"
import           Xmobar


config :: Config
config = defaultConfig
  { font             = "xft:SpaceMono Nerd Font-13"
  , additionalFonts  = []
  , borderColor      = "#000000ff"
  , borderWidth      = 4
  -- , border           = FullBM 4
  -- , alpha            = 255
  -- , bgColor          = "#3C435E"
  , alpha            = 64
  -- , bgColor          = "#000000" -- Somehow color get messed up when using alpha issue#246 this is the workaround. invert 3rd4th <--> 5th6th like this.
  , fgColor          = "#ffffff"
  , position         = TopSize L 100 30
  , textOffset       = -1
  , iconOffset       = 13
  , lowerOnStart     = True
  , pickBroadest     = False
  , persistent       = False
  , hideOnStart      = False
  , iconRoot         = "/etc/icons"
  , allDesktops      = True
  , overrideRedirect = True
  , commands         = [ Run $ Cpu ["-t", "<total>%"] 10
                       , Run $ Memory ["<usedratio>MB"] 10
                       , Run $ Date "%a %m/%_d" "date" 10
                       , Run $ Date "%H:%M:%S" "time" 10
                       , Run $ BatteryN
                         ["BAT0"]
                         [ "-t"
                         , "<acstatus>  <left>"
                         , "-S"
                         , "Off"
                         , "-d"
                         , "0"
                         , "-m"
                         , "3"
                         , "-L"
                         , "10"
                         , "-H"
                         , "90"
                         , "-p"
                         , "3"
                         , "--low"
                         , "#f07178"
                         , "--normal"
                         , "#676E95"
                         , "--high"
                         , "#80cbc4"
                         , "--"
                         , "-P"
                         , "-a"
                         , "notify-send -u critical 'Battery running out!!!!!!'"
                         , "-A"
                         , "7"
           -- Charged
                         , "-i"
                         , "<icon=battery-charging.xpm/>"
           -- AC on
                         , "-O"
                         , "<icon=battery-charging.xpm/>"
           -- Discharging
                         , "-o"
                         , "<icon=battery.xpm/>"
                         , "-H"
                         , "10"
                         , "-L"
                         , "7"
                         , "-h"
                         , "#80cbc4"
                         , "-l"
                         , "#f07178"
                         ]
                         50
                         "battery"
                       , Run $ Volume
                         "default"
                         "Master"
                         [ "-t"
                         , "<status>  <volume>%"
                         , "--"
                         , "-O"
                         , "<icon=volume.xpm/>"
                         , -- on
                           "-o"
                         , "<icon=volume-mute.xpm/>"
                         , -- off
                           "-C"
                         , "#FFFFFF"
                         , "-c"
                         , "#f07178"
                         ]
                         3
                       , Run StdinReader
                       -- , Run Brightness
                          -- [ "-t", "<ipat>"
                          -- , "--"
                          -- , "--brightness-icon-pattern", "<icon=bright_%%.xpm/>"
                          -- ] 30
                       ]
  , sepChar          = "%"
  , alignSep         = "}{"
  , template         =
    " <fc=#ffffff>\xe61f</fc> %StdinReader% }\
     \ <fc=#f78c6c>%date%</fc> <fc=#89ddff>%time%</fc> \
     \{<fc=#ffffff,#0000ff> %battery% \xe266 %cpu% \63578 %memory% %default:Master% \62227</fc> "
  }


main :: IO ()
main = xmobar config
