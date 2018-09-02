Config { font = "-misc-fixed-*-*-*-*-14-*-*-*-*-*-*-*"
       , additionalFonts = []
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 255
       , position = Top
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Weather "SBPK" ["-t","<station>: <tempC>C",
                                          "-L","18","-H","25",
                                          "--normal","green",
                                          "--high","red",
                                          "--low","lightblue"] 36000
                    , Run Network "wlp3s0" ["-L","0","-H","32",
                                          "--normal","green","--high","red"] 10
                    , Run Cpu ["-L","3","-H","50",
                               "--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Com "uname" ["-s","-r"] "" 36000
                    , Run Date "%a %_d %b %Y %H:%M:%S" "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template =
         "<action=`xdotool key \-\-clearmodifiers Super_L+Tab`> <fc=#00DDFF>[W]</fc> </action>\
         \| <action=`gnome-terminal \-\- htop`>%cpu%</action> \
         \| %memory% * %swap% \
         \| %wlp3s0% }\
         \{ <fc=#ee9a00>%date%</fc> \
         \| %SBPK% \
         \| <action=`xdotool key \-\-clearmodifiers Super_L+Shift_L+Return`>%uname%</action> \
         \| <action=`xdotool key \-\-clearmodifiers Super_L+Shift_L+c`><fc=#FF0000>[X]</fc></action>"
       }

