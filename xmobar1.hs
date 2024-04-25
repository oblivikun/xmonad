Config { font = "Roboto Bold 10"
       , additionalFonts =
          [ "FontAwesome 12"
          , "FontAwesome Bold 8"
          , "FontAwesome 14"
          , "Hack 19"
          , "Hack 14"
          ]
       , border = NoBorder
       , bgColor = "#2B2E37"
       , fgColor = "#929AAD"
       , alpha = 255
       , position = TopSize L 100 40
       , lowerOnStart = True
       , allDesktops = True
       , persistent = False
       , hideOnStart = False
       , iconRoot = "/home/erel/.xmonad/icons/"
       , commands =
         [ Run UnsafeXPropertyLog "_XMONAD_LOG_0"
         , Run Date "%a, %d %b   <fn=5>󰥔</fn>     %H:%M:%S" "date" 10
         , Run Memory ["-t","Mem: <fc=#AAC0F0><usedratio></fc>%"] 10
         , Run Com "/home/erel/.xmonad/cpu_temp.sh" [] "cpu" 10
         , Run Com "/home/erel/.xmonad/volume.sh" [] "volume" 10
         , Run Com "/home/erel/.xmonad/wifi.sh" [] "network" 10
         , Run Com "/home/erel/.xmonad/trayer-padding.sh" [] "trayerpad" 10
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "\
            \ %_XMONAD_LOG_0%   \
            \}\
            \<action=xdotool key super+r>%date%</action>\
            \{\
            \<action=xdotool key super+y>\
            \     \
            \%memory%\
            \     \
            \|\
            \     \
            \%cpu%\
            \     \
            \|\
            \</action>\
            \|\
            \   \
            \%trayerpad%"
       }
