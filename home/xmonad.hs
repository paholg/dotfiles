-- Note for arch: May need to run `ghc-pkg recache` sometimes to get
-- `xmonad --recompile` to work again

import Data.List
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops(ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Gaps
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.Hacks (fixSteamFlicker)
import qualified XMonad.StackSet as W

-- Spacemacs dark-mode colors
black =   "#0a0814"
red =     "#f2241f"
green =   "#67b11d"
yellow =  "#b1951d"
blue =    "#4f97d7"
magenta = "#a31db1"
cyan =    "#2d9574"
white =   "#a3a3a3"

------------------------------------------------------------
-- set up workspace info
ws_list = ["`", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "="]
my_workspaces = ["^ca(1,xdotool key super+"++cmd++") "++display++" ^ca()" |
                 ws <- ws_list,
                 let display = ws,
                 let cmd = if ws == "`" then "grave"
                           else if ws == "-" then "minus"
                           else if ws == "=" then "equal"
                           else ws
                ]
wsLogHook h = dynamicLogWithPP $ def {
  ppCurrent = dzenColor black cyan,
  ppVisible = dzenColor black white,
  ppHidden = dzenColor white black . noScratchPad,
  ppUrgent = dzenColor black red,
  ppOrder = \(ws:l:t:_) ->
  [ ws,
    "^ca(1,xdotool key super+space)^fg(" ++ blue ++ ")" ++ layout l ++"^ca()",
    "^ca(1,xdotool key super+Tab) "++ t ++"^ca()"
  ],
  ppTitle = dzenColor blue black,
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
my_terminal = "alacritty"
my_pdfviewer = "okular"
my_statusbar = "rustybar"

------------------------------------------------------------
-- layouts

tall = renamed [Replace "Tall"] $ ResizableTall 1 (1/20) (10/20) []
-- mirror = renamed [Replace "Mirror" ] $ ResizableTall 2 (4/100) (89/100) []
-- big = renamed [Replace "Big"] $ ResizableTall 1 (1/20) (18/20) []
-- cal = renamed [Replace "Cal"] $ ResizableTall 2 (1/20) (15/20) []
-- three = ThreeCol 1 (4/100) (1/2)

main_layout = windowNavigation (tall)
-- big_layout = windowNavigation (big)
-- cal_layout = windowNavigation (cal)
-- mirror_layout = windowNavigation (mirror)
-- trip_layout = windowNavigation(three)

space = 5

layout = (noBorders . avoidStruts . spacing space . gaps [(U, -space), (R, -space), (L, -space), (D, -space)] $ main_layout) ||| (noBorders . avoidStruts $ Full)
  -- ||| (avoidStruts $ mirror_layout)

------------------------------------------------------------
-- adjust program behavior
myManageHook = composeAll . concat $
  [[
      isFullscreen --> doFullFloat,
      isDialog --> doFloat
   ],
   -- general things
   [className =? i --> doFloat | i <- floats],
   [className =? i --> doCenterFloat | i <- center_floats],
   [className =? i --> doIgnore | i <- ignores],
   [title =? i --> doFullFloat | i <- full_floats_by_title]
  ]
  where
    floats = ["MainThrd"]
    center_floats = ["xmessage", "Tk", "TVTK Scene"]
    ignores = ["Xfce4-notifyd"]
    full_floats = []
    full_floats_by_title = []

-- Just zoom things
-- Taken from https://www.peterstuart.org/posts/2021-09-06-xmonad-zoom/
manageZoomHook =
  composeAll $
    [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat,
      (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
    ]
  where
    zoomClassName = "zoom"
    tileTitles =
      [ "Zoom - Free Account", -- main window
        "Zoom - Licensed Account", -- main window
        "Zoom", -- meeting window on creation
        "Zoom Meeting" -- meeting window shortly after creation
      ]
    shouldFloat title = title `notElem` tileTitles
    shouldSink title = title `elem` tileTitles
    doSink = (ask >>= doF . W.sink) <+> doF W.swapDown

------------------------------------------------------------
-- scratch pads
scratch_pads = [ NS "terminal" spawnTerm findTerm manageTerm,
                 NS "bitwarden" spawnBitwarden findBitwarden manageBitwarden,
                 NS "volume" spawnVolume findVolume manageVolume,
                 NS "calc" spawnCalc findCalc manageCalc,
                 NS "network" spawnNetwork findNetwork manageNetwork
               ]
  where
    spawnTerm = my_terminal ++ " --title scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.3
        w = 0.5
        t = 1.0 - h
        l = 0.0
    spawnBitwarden = my_terminal ++ " --title bitwarden"
    findBitwarden = title =? "bitwarden"
    manageBitwarden = customFloating $ W.RationalRect l t w h
      where
        h = 1
        w = 0.5
        t = 1.0 - h
        l = (1.0 - w)/2.0
    spawnVolume = "pavucontrol"
    findVolume = title =? "Volume Control"
    manageVolume = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.5
        t = (1.0 - h)/2.0
        l = (1.0 - w)/2.0
    spawnCalc = my_terminal ++ " --title calculator -e irb"
    findCalc = title =? "calculator"
    manageCalc = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.2
        t = 0.0
        l = 1.0 - w
    spawnNetwork = my_terminal ++ " --title nmtui -e nmtui"
    findNetwork = title =? "nmtui"
    manageNetwork = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.5
        t = (1.0 - h)/2.0
        l = (1.0 - w)/2.0

------------------------------------------------------------
my_keys = [
  -- PrntScr for full screen, Shift+PrntScr for window, Ctrl+PrintScr to click box
  ("C-<Print>", spawn "sleep 0.2; scrot -s $HOME/screenshots/%F-%H%M%S-s.png"),
  ("S-<Print>", spawn "scrot -u $HOME/screenshots/%F-%H%M%S-f.png"),
  ("<Print>", spawn "scrot $HOME/screenshots/%F-%H%M%S.png"),
  -- multi-monitor modes
  ("M-m",  spawn "monitor_switch default"),
  ("M-C-m",  spawn "monitor_switch ctrl"),
  ("M-S-m",  spawn "monitor_switch shift"),
  ("M-C-S-m",  spawn "monitor_switch ctrl-shift"),
  -- audio modes
  ("M-S-s", spawn "set_sink 'Audioengine HD3'"),
  ("M-S-h", spawn "set_sink 'KT USB Audio'"),
  -- run programs
  ("M-t", spawn my_terminal),
  ("M-r", spawn "dmenu_run -i -nb black -sb grey -nf grey -sf black -fn '-misc-fixed-medium-r-normal--18-*-*-*-*-*-*-*'"),
  -- window manager stuff
  ("M-v", sendMessage ToggleStruts),
  ("M-<Space>", sendMessage NextLayout),
  ("M-q", spawn ""), -- unbind this key
  ("M-C-q", kill),
  ("M-C-l", spawn "locker"),
  ("M-M1-x", spawn "xkill"),
  ("M-S-q", sequence_ [spawn "killall rustybar", restart "xmonad" True]),
  ("M-M1-q", io (exitWith ExitSuccess)), -- exit xmonad
  ("M-,", sendMessage (IncMasterN 1)), -- increase windows on left
  ("M-.", sendMessage (IncMasterN (-1))), -- decrease windows on left
  ("M-[", prevScreen),
  ("M-]", nextScreen),
  ("M-S-[", shiftNextScreen),
  ("M-S-]", shiftPrevScreen),
  ("M-z", withFocused $ windows . W.sink), -- push window into tiling
  ("M-e", sendMessage $ Go U), -- change focus using wasd
  ("M-s", sendMessage $ Go L),
  ("M-d", sendMessage $ Go D),
  ("M-f", sendMessage $ Go R),
  ("M-h", sendMessage Shrink), -- move center to left
  ("M-j", sendMessage MirrorShrink), -- increase vertical size
  ("M-k", sendMessage MirrorExpand), -- reduce vertical size
  ("M-l", sendMessage Expand), -- move center to right
  -- Scratch pads
  ("M-o", namedScratchpadAction scratch_pads "volume"),
  ("M-c", namedScratchpadAction scratch_pads "calc"),
  ("M-x", namedScratchpadAction scratch_pads "terminal"),
  ("M-p", namedScratchpadAction scratch_pads "bitwarden"),
  -- Media keys
  ("<XF86AudioMute>",         spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
  ("M-<Left>",                spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
  ("<XF86AudioMicMute>",      spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle"),
  ("M-<Right>",               spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle"),
  ("<XF86AudioLowerVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
  ("<XF86AudioRaiseVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
  ("M-<Down>",                spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
  ("M-<Up>",                  spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
  ("<XF86MonBrightnessUp>",   spawn "brightnessctl s +10%"),
  ("<XF86MonBrightnessDown>", spawn "brightnessctl s 10%-")
  ] ++ -- workspace switching
          [("M-" ++ name, windows $ W.greedyView ws) | (name, ws) <- zip ws_list my_workspaces] ++
          [("M-S-" ++ name, windows $ W.shift ws) | (name, ws) <- zip ws_list my_workspaces] ++
          [("M-C-" ++ name, windows $ copy ws) | (name, ws) <- zip ws_list my_workspaces]

startup = do
  setWMName "LG3D" -- makes java apps work
  spawnOnce "firefox"

main = do
  my_statusbar <- spawnPipe my_statusbar
  xmonad $ ewmh $ def {
    workspaces = my_workspaces,
    startupHook = startup,
    manageHook = manageDocks
      <+> myManageHook
      <+> manageZoomHook
      <+> manageHook def
      <+> namedScratchpadManageHook scratch_pads,
    handleEventHook = fixSteamFlicker <+> docksEventHook <+> handleEventHook def,
    layoutHook = layout,
    logHook = wsLogHook my_statusbar,
    borderWidth = 2,
    normalBorderColor = blue,
    focusedBorderColor = cyan,
    modMask = mod4Mask, -- rebind Mod to Windows key
    terminal = my_terminal
  } `additionalKeysP` (my_keys)
