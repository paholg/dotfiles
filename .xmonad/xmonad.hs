import qualified XMonad.StackSet as W
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops(ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout.NoBorders
import XMonad.Hooks.SetWMName
import XMonad.Actions.CopyWindow
import System.IO
import System.Exit
import XMonad.Actions.CopyWindow
import XMonad.Actions.GridSelect
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Actions.PhysicalScreens
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiColumns
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.UrgencyHook
import Data.List


-- some colors -----------------------------------
black = "#000000"
white = "#FFFFFF"
lightgray = "#AAAAAA"
gray = "#777777"
slategray = "#708090"
red = "#C11B17"
green = "#347C17"
blue = "#00688B"
yellow = "#FFFF00"
palegreen = "#98fb98"
purple = "#a020f0"
palevioletred = "#db7093"
thistle = "#d8bfd8"
orchid = "#da70d6"
sienna = "#a0522d"
darkgoldenrod = "#b8860b"
skyblue = "#87ceeb"

------------------------------------------------------------
-- set up workspace info
ws_list = ["`", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "="]
my_workspaces = ["^ca(1,xdotool key super+"++cmd++") "++display++" ^ca()" |
                 ws <- ws_list,
                 let display = if ws == "`" then "~" else ws,
                 let cmd = if ws == "`" then "grave"
                           else if ws == "-" then "minus"
                           else if ws == "=" then "equal"
                           else ws
                ]
wsLogHook h = dynamicLogWithPP $ defaultPP {
  ppCurrent = dzenColor black green,
  ppVisible = dzenColor black gray,
  ppHidden = dzenColor gray black . noScratchPad,
  ppUrgent = dzenColor black red,
  ppOrder = \(ws:l:t:_) ->
  [ ws,
    "^ca(1,xdotool key super+space)^fg(" ++ blue ++ ")" ++ layout l ++"^ca()",
    "^ca(1,xdotool key super+Tab) "++ t ++"^ca()"
  ],
  ppTitle = dzenColor skyblue black,
  ppSep = " ",
  ppWsSep = "",
  ppOutput = hPutStrLn h
  }
  where
    noScratchPad ws = if ws == "NSP" then "" else ws
    layout l = if "Tall" `isInfixOf` l then "^ro(18x20)^ro(18x20)"
               else if "Big" `isInfixOf` l then "^ro(28x20)^ro(8x20)"
               else if "Full" `isInfixOf` l then "^ro(36x20)"
               else if "ThreeCol" `isInfixOf` l then "^ro(12x20)^ro(12x20)^ro(12x20)"
               else l

------------------------------------------------------------
-- programs to use
my_terminal = "xfce4-terminal"
my_pdfviewer = "zathura"
my_statusbar = "dzen2 -fn Monospace-10 -bg black -ta l -xs 0"
-- my_statusbar = "~/projects/rustybar/target/release/rustybar &>> ~/.config/rustybar/log"


------------------------------------------------------------
-- layouts

tall = renamed [Replace "Tall"] $ ResizableTall 1 (1/20) (10/20) []
-- mirror = renamed [Replace "Mirror" ] $ ResizableTall 2 (4/100) (89/100) []
big = renamed [Replace "Big"] $ ResizableTall 1 (1/20) (18/20) []
cal = renamed [Replace "Cal"] $ ResizableTall 2 (1/20) (15/20) []
three = ThreeCol 1 (4/100) (1/2)

big_layout = windowNavigation (big)
cal_layout = windowNavigation (cal)
main_layout = windowNavigation (tall)
-- mirror_layout = windowNavigation (mirror)
trip_layout = windowNavigation(three)

space = 5

layout = (avoidStruts . spacing space . gaps [(U, -space), (R, -space), (L, -space), (D, -space)] $
          onWorkspace (my_workspaces!!3) big_layout $
          onWorkspace (my_workspaces!!11) cal_layout $
          onWorkspace (my_workspaces!!12) big_layout $
          main_layout) ||| Full -- ||| (avoidStruts $ mirror_layout)

-- xmobarLogHook h = dynamicLogWithPP $ xmobarPP
--   { ppOutput = hPutStrLn h,
--     ppTitle = xmobarColor "#3CB371" "" . shorten 100,
--     ppHidden = noScratchPad
--   }
--   where
--     noScratchPad ws = if ws == "NSP" then "" else ws

-- -- If q contains x
-- contains q x = fmap (isInfixOf x) q

------------------------------------------------------------
-- adjust program behavior
myManageHook = composeAll . concat $
  [[
      isFullscreen --> doFullFloat,
      isDialog --> doFloat,

      -- put startup programs on their respective workspaces
      className =? "Google-chrome" --> doShift (my_workspaces!!3),

      title =? "gcal" --> doShift (my_workspaces!!11),

      className =? "Rhythmbox" --> doShift (my_workspaces!!12),
      className =? "Pidgin" --> doShift (my_workspaces!!12),
      title =? "htop" --> doShift (my_workspaces!!11),

      title =? "Starcraft II" --> doFloat
   ],
   -- general things
   [className =? i --> doFloat | i <- floats],
   [className =? i --> doCenterFloat | i <- center_floats],
   [className =? i --> doIgnore | i <- ignores],
   [title =? i --> doFullFloat | i <- full_floats_by_title]
  ]
  where
    floats = ["Steam", "MainThrd"]
    center_floats = ["xmessage", "Tk", "TVTK Scene"]
    ignores = ["Xfce4-notifyd"]
    full_floats = ["steam"]
    full_floats_by_title = ["Kerbal Space Program", "Starcraft II"]

------------------------------------------------------------
-- scratch pads
my_scratch_pads = [ NS "terminal" spawnTerm findTerm manageTerm,
                    NS "volume" spawnVolume findVolume manageVolume,
                    NS "calc" spawnCalc findCalc manageCalc,
                    NS "clips" spawnClips findClips manageClips,
                    NS "network" spawnNetwork findNetwork manageNetwork
                  ]
  where
    spawnTerm = my_terminal ++ " --title scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.3
        w = 0.5
        t = 1 - h
        l = 0
    spawnVolume = "pavucontrol"
    findVolume = title =? "Volume Control"
    manageVolume = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.5
        t = (1 - h)/2
        l = (1 - w)/2
    spawnCalc = my_terminal ++ " --title calculator -e \"ipython3 --pylab\""
    findCalc = title =? "calculator"
    manageCalc = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.2
        t = 0.02
        l = 1 - w
    spawnClips = my_terminal ++ " --title clips-player -e clips"
    findClips = title =? "clips-player"
    manageClips = customFloating $ W.RationalRect l t w h
      where
        h = 0.7
        w = 0.3
        t = 0.015
        l = 1 - w
    spawnNetwork = my_terminal ++ " --title nmtui -e nmtui"
    findNetwork = title =? "nmtui"
    manageNetwork = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.5
        t = (1 - h)/2
        l = (1 - w)/2

------------------------------------------------------------
-- grid select
myGSConfig = defaultGSConfig {gs_cellheight = 50, gs_cellwidth = 100}

my_keys = [
  ("M-'", windows copyToAll),
  -- PrntScr for full screen, Shift+PrntScr for window, Ctrl+PrintScr to click box
  ("C-<Print>", spawn "sleep 0.2; scrot -s /home/paho/screenshots/%F-%H%M%S-s.png"),
  ("S-<Print>", spawn "scrot -u /home/paho/screenshots/%F-%H%M%S-f.png"),
  ("<Print>", spawn "scrot /home/paho/screenshots/%F-%H%M%S.png"),
  ("M-S-u", spawn "upload-ss"),
  -- multi-monitor modes
  ("M-m", sequence_ [spawn "/home/paho/.screenlayout/all.sh && background", restart "xmonad" True]),
  ("M-S-m", sequence_ [spawn "/home/paho/.screenlayout/dup.sh && background", restart "xmonad" True]),
  ("M-C-m", sequence_ [spawn "/home/paho/.screenlayout/solo.sh && background", restart "xmonad" True]),
  -- background stuff
  ("M-b", spawn "bg-swap"),
  ("M-S-b", spawn "bg-last"),
  ("M-S-n", spawn "bg-next"),
  ("M-n", spawn "bg-newest"),
  ("M-C-d", spawn "bg-rm"),
  -- power stuff
  ("M-C-S-z", spawn "sudo pm-suspend"),
  ("M-C-h", spawn "sudp pm-hibernate"),
  -- run programs
  ("M-e", spawn my_terminal),
  ("M-f", spawn my_pdfviewer),
  ("M-r", spawn "em"),
  ("M-u", spawn "pavucontrol"),
  ("M-p", spawn "dmenu_run -i -nb black -sb grey -nf grey -sf black -fn '-misc-fixed-medium-r-normal--18-*-*-*-*-*-*-*'"),
  -- window manager stuff
  ("M-v", sendMessage ToggleStruts), -- toggle covering xmobar
  ("M-<Space>", sendMessage NextLayout), -- swap layouts
  ("M-q", kill), -- kill focused window
  ("M-M1-x", spawn "xkill"),
  ("M-M1-q", spawn "killall rustybar"),
  ("M-S-q", restart "xmonad" True), -- refresh xmonad
  ("M-C-q", io (exitWith ExitSuccess)), -- exit xmonad
  ("M-C-l", spawn "dm-tool lock"), -- lock session
  ("M-j", sendMessage Shrink), -- move center to left
  ("M-l", sendMessage Expand), -- move center to right
  ("M-,", sendMessage (IncMasterN 1)), -- increase windows on left
  ("M-.", sendMessage (IncMasterN (-1))), -- decrease windows on left
  ("M-[", viewScreen 0), -- change focus to screen 0
  ("M-]", viewScreen 1), -- change focus to creen 1
  ("M-S-[", sendToScreen 0), -- move window to screen 0
  ("M-S-]", sendToScreen 1), -- move window to screen 1
  ("M-g", goToSelected myGSConfig), -- GridSelect
  ("M-t", withFocused $ windows . W.sink), -- push window into tiling
  ("M-w", sendMessage $ Go U), -- change focus using wasd
  ("M-a", sendMessage $ Go L),
  ("M-s", sendMessage $ Go D),
  ("M-d", sendMessage $ Go R),
  ("M-S-w", sendMessage $ Swap U), -- swap windows using shift + wasd
  ("M-S-a", sendMessage $ Swap L),
  ("M-S-s", sendMessage $ Swap D),
  ("M-S-d", sendMessage $ Swap R),
  ("M-i", sendMessage MirrorExpand), -- reduce vertical size
  ("M-k", sendMessage MirrorShrink), -- increase vertical size
  ("M-z", spawn "transset-df -a -t 0.85"),
  -- Scratch pads
  ("M-o", namedScratchpadAction my_scratch_pads "volume"),
  ("M-c", namedScratchpadAction my_scratch_pads "calc"),
  ("M-y", namedScratchpadAction my_scratch_pads "clips"),
  ("M-h", namedScratchpadAction my_scratch_pads "network"),
  ("M-x", namedScratchpadAction my_scratch_pads "terminal"),
  -- Media keys
  ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle"),
  ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -3%"),
  ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +3%"),
  ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10"),
  ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  ] ++ -- workspace switching
          [("M-" ++ name, windows $ W.greedyView ws) | (name, ws) <- zip ws_list my_workspaces] ++
          [("M-S-" ++ name, windows $ W.shift ws) | (name, ws) <- zip ws_list my_workspaces] ++
          [("M-C-" ++ name, windows $ copy ws) | (name, ws) <- zip ws_list my_workspaces]

main = do
  my_statusbar <- spawnPipe my_statusbar
  xmonad $ ewmh $ withUrgencyHook NoUrgencyHook defaultConfig {
    workspaces = my_workspaces,
    startupHook = setWMName "LG3D", -- makes java apps work
    manageHook =  manageDocks <+> myManageHook <+> manageHook defaultConfig
                  <+> namedScratchpadManageHook my_scratch_pads,
    layoutHook = layout,
    logHook = wsLogHook my_statusbar,
    borderWidth = 0,
    normalBorderColor = "black",
    focusedBorderColor = skyblue,
    --focusedBorderColor = black,
    modMask = mod4Mask, -- rebind Mod to Windows key
    terminal = my_terminal
    } `additionalKeysP` (my_keys)
