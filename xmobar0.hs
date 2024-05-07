
Config {
       font = "xft:Zekton:size=12:bold:antialias=true"
       , additionalFonts = [ "xft:Fira Code Nerd Font:size=15" ]
       , allDesktops = True
       , bgColor = "#FFFFFF"
       , fgColor = "#000000"
       , alpha = 120
       , position = TopSize L 100 40
       , commands = [ Run Cpu [ "--template", "<fc=#a9a1e1><fn=1></fn></fc> <total>%"
                              , "--Low","3"
                              , "--High","50"
                              , "--low","#000000"
                              , "--normal","#000000"
                              , "--high","#000000"] 50

                    , Run Memory ["-t","<fc=#51afef><fn=1></fn></fc> <usedratio>%"
                                 ,"-H","80"
                                 ,"-L","10"
                                 ,"-l","#000000"
                                 ,"-n","#000000"
                                 ,"-h","#000000"] 50

                    , Run Date "<fc=#ECBE7B><fn=1></fn></fc> %a %b %_d %I:%M" "date" 300
                    , Run DynNetwork ["-t","<fc=#4db5bd><fn=1></fn></fc> <rx>, <fc=#c678dd><fn=1></fn></fc> <tx>"
                                     ,"-H","200"
                                     ,"-L","10"
                                     ,"-h","#000000"
                                     ,"-l","#000000"
                                     ,"-n","#000000"] 50

                    , Run CoreTemp ["-t", "<fc=#CDB464><fn=1></fn></fc> <core0>°"
                                   , "-L", "30"
                                   , "-H", "75"
                                   , "-l", "#000000"
                                   , "-n", "#000000"
                                   , "-h", "#aa4450"] 50

                    -- battery monitor
                    , Run BatteryP       [ "BAT0" ]
                                         [ "--template" , "<fc=#B1DE76><fn=1></fn></fc> <acstatus>"
                                         , "--Low"      , "10"        -- units: %
                                         , "--High"     , "80"        -- units: %
                                         , "--low"      , "#000000" -- #ff5555
                                         , "--normal"   , "#000000"
                                         , "--high"     , "#000000"

                                         , "--" -- battery specific options
                                                   -- discharging status
                                                   , "-o"   , "<left>% (<timeleft>)"
                                                   -- AC "on" status
                                                   , "-O"   , "<left>% (<fc=#98be65>Charging</fc>)" -- 50fa7b
                                                   -- charged status
                                                   , "-i"   , "<fc=#98be65>Charged</fc>"
                                         ] 50
                    , Run UnsafeXPropertyLog "_XMONAD_LOG_0"
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       ,  template = " <fc=#25390f><fn=5></fn></fc> %_XMONAD_LOG_0% }{ %cpu% | %coretemp% | %memory% | %battery% | %dynnetwork% | %date% |"
       }
