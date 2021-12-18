-- This file is generated from "README.org"
import           Control.Arrow                        (first)
import qualified Data.Map                             as M
import           System.Exit                          (exitSuccess)
import           XMonad                               hiding ((|||))
import           XMonad.Actions.Navigation2D          (Direction2D (D, L, R, U),
                                                       screenSwap, windowGo,
                                                       windowSwap,
                                                       windowToScreen,
                                                       withNavigation2DConfig)

import qualified Codec.Binary.UTF8.String             as UTF8
import qualified DBus                                 as D
import qualified DBus.Client                          as D
import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Actions.GridSelect
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.ShowText
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WorkspaceNames        (getCurrentWorkspaceName)
import           XMonad.Config.Prime                  (Window)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops            (ewmh)
import           XMonad.Hooks.ManageDocks             (avoidStruts, docks)
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Place
import           XMonad.Layout.AutoMaster
import           XMonad.Layout.BinarySpacePartition   (Rotate (Rotate),
                                                       Swap (Swap), emptyBSP)
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid                   (Grid (..))
import qualified XMonad.Layout.IndependentScreens     as LIS
import           XMonad.Layout.LayoutCombinators      (JumpToLayout (JumpToLayout),
                                                       (|||))
import           XMonad.Layout.NoBorders              (noBorders, smartBorders)
import           XMonad.Layout.ResizableTile          (ResizableTall (..))
import           XMonad.Layout.Spacing                (Border (Border),
                                                       spacingRaw)
import           XMonad.Layout.Spiral                 (spiral)
import           XMonad.Layout.StateFull
import           XMonad.Layout.Tabbed                 (simpleTabbed)
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.TwoPane                (TwoPane (..))
import           XMonad.Operations
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt          (confirmPrompt)
import           XMonad.Prompt.FuzzyMatch             (fuzzyMatch)
import           XMonad.Prompt.Man                    (manPrompt)
import           XMonad.Prompt.Shell                  (shellPrompt)
import           XMonad.Prompt.Unicode                (mkUnicodePrompt)
import qualified XMonad.StackSet                      as W
import           XMonad.Util.Cursor                   (setDefaultCursor)
import           XMonad.Util.EZConfig                 (additionalKeysP)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.PureX
import           XMonad.Util.Run                      (hPutStrLn, spawnPipe)
import           XMonad.Util.SpawnOnce                (spawnOnce)
import           XMonad.Util.WindowProperties
myKeys :: [(String, X ())]
myKeys =
  [
    ("M-<Return>", do
      w <- gets (W.currentTag . windowset)
      let property = Title $ "alacritty-workspace-" ++ w
      alacritty_windows_workspace <- allWithProperty property
      new_terminal <- focusedHasProperty property
      if new_terminal
         then spawn (myTerminal ++ " -e tmux")
         else if 0 == length alacritty_windows_workspace
            then spawn (myTerminal ++ " --title=alacritty-workspace-" ++ w ++ " -e tmuxinator s workspace-" ++ w)
            else windows $ (W.focusWindow $ alacritty_windows_workspace !! 0)
    )
  , ("M-S-c", kill)
  , ("M-c", kill)
  , ("M-Delete", kill)
  , ("M-o", spawn "betterlockscreen -l & /run/current-system/sw/bin/systemctl suspend && fg") -- lock screen
  , ("M-S-q", confirmPrompt myXPConfig "exit" $ io exitSuccess) -- prompt to kill xmonad
  , ("M-q", spawn "xmonad --restart")

  , ("M-b", spawn "firefox")
  , ("M-p", spawn "gpaste-client ui")
  , ("M-i", spawn "kdeconnect-app")
  , ("M-v", spawn "qv2ray")
  , ("M-n", spawn "nautilus")
  , ("M-S-n", spawn "dolphin")
  , ("M-S-z", spawn "alacritty -e zsh")

  , ("M-x", namedScratchpadAction myScratchPads "terminal")
  , ("M-z", spawn "~/.config/eww/scripts/trigger")

  -- , ("M-d", shellPrompt myXPConfig)
  , ("M-d", spawn "rofi -combi-modi window,drun -show combi -modi combi -theme ~/.config/rofi/apps.css")
  , ("M-f", spawn "rofi -combi-modi window -show combi -modi combi -theme ~/.config/rofi/apps.css")
  , ("M-r", spawn "rofi -combi-modi run -show combi -modi combi -theme ~/.config/rofi/apps.css")
  , ("C-M-l", spawn "light-locker")
  , ("M-<Esc>", nextMatch Forward isOnAnyVisibleWS)
  , ("M-<Tab>", nextMatch History (return True))


  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")

  , ("<Print>", spawn "gnome-screenshot -a -i")

  , ("M-<Space>", sendMessage NextLayout) -- Change to next layout in order

  , ("M-S-t", withFocused $ windows . W.sink) -- unfloat window
  , ("M-a", placeFocused $ underMouse (0,0))

  , ("M-<Page_Down>", moveTo Next NonEmptyWS)
  , ("M-<Page_Up>", moveTo Prev NonEmptyWS)
  , ("M-S-<Page_Down>", shiftToNext >> nextWS)
  , ("M-S-<Page_Up>", shiftToPrev >> prevWS)

  , ("M-<KP_Page_Down>", moveTo Next NonEmptyWS)
  , ("M-<KP_Page_Up>", moveTo Prev NonEmptyWS)
  , ("M-S-<KP_Page_Down>", shiftToNext >> nextWS)
  , ("M-S-<KP_Page_Up>", shiftToPrev >> prevWS)

  , ("M-h", windowGo L False)
  , ("M-l", windowGo R False)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-g", windows W.focusMaster)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-S-<Return>", windows W.swapMaster)

  , ("M-S-h", sendMessage Shrink)
  , ("M-S-l", sendMessage Expand)

   -- Directional navigation of windows
   , ("M-<R>", windowGo R False)
   , ("M-<L>", windowGo L False)
   , ("M-<U>", windowGo U False)
   , ("M-<D>", windowGo D False)

   , ("C-M-<R>", windowToScreen R False)
   , ("C-M-<L>", windowToScreen L False)
   , ("C-M-<U>", windowToScreen U False)
   , ("C-M-<D>", windowToScreen D False)

   , ("M-S-<R>", screenSwap R False)
   , ("M-S-<L>", screenSwap L False)
   , ("M-S-<U>", screenSwap U False)
   , ("M-S-<D>", screenSwap D False)

  , ("M-,", do
        layout <- getActiveLayoutDescription
        case layout of
          "Spacing BSP" -> sendMessage Swap
          _             -> sendMessage $ IncMasterN 1
    )
  , ("M-.", do
        layout <- getActiveLayoutDescription
        case layout of
          "Spacing BSP" -> sendMessage Rotate
          _             -> sendMessage $ IncMasterN (-1)
    )
  ]
myXPKeymap =
  M.fromList
    $  map
         (first $ (,) 0)
         [ (xK_Return   , setSuccess True >> setDone True)
         , (xK_KP_Enter , setSuccess True >> setDone True)
         , (xK_BackSpace, deleteString Prev)
         , (xK_Delete   , deleteString Prev)
         , (xK_Left     , moveCursor Prev)
         , (xK_Right    , moveCursor Next)
         , (xK_Down     , moveHistory W.focusUp')
         , (xK_Up       , moveHistory W.focusDown')
         , (xK_Escape   , quit)
         ]
    ++ map (first $ (,) controlMask) [(xK_v, pasteString)]
myXPConfig = def { font              = myFont
                 , bgColor           = "#232635"
                 , fgColor           = "#A6ACCD"
                 , bgHLight          = "#444267"
                 , fgHLight          = "#A6ACCD"
                 , borderColor       = "#de766c"
                 , promptKeymap      = myXPKeymap
                 , promptBorderWidth = 0
                 , position          = Top
                 , height            = myPromptHeight
                 , autoComplete      = Nothing
                 , searchPredicate   = fuzzyMatch
                 , alwaysHighlight   = True
                 }
myEmojiXPConfig = def { font              = myEmojiFont
                      , bgColor           = "#232635"
                      , fgColor           = "#A6ACCD"
                      , bgHLight          = "#444267"
                      , fgHLight          = "#A6ACCD"
                      , borderColor       = "#d6778c"
                      , promptKeymap      = myXPKeymap
                      , promptBorderWidth = 0
                      , position          = Top
                      , height            = myPromptHeight
                      , autoComplete      = Nothing
                      , searchPredicate   = fuzzyMatch
                      , alwaysHighlight   = True
                      }
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = True

myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
  [ ( (modm, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )

    -- mod-button2, Raise the window to the top of the stack
  , ((modm, button2)
    , \w -> focus w >> kill
    )

    -- mod-button3, Set the window to floating mode and resize by dragging
  , ( (modm, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]
myScratchPads =
  [ NS "terminal" spawnTerm    findTerm   manageTerm
  ]
 where
  centralh   = 0.9
  centralw   = 0.9
  centralt   = 0.95 - centralh
  centrall   = 0.95 - centralw

  spawnTerm  = myTerminal ++ " --title=alacritty-drop --config-file=${HOME}/.config/alacritty/alacritty-drop.yml "
  findTerm   = title =? "alacritty-drop"
  manageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.905
    w = 0.9
    t = 0.05
    l = (1 - w) / 2

  managevifm =
    customFloating $ W.RationalRect centrall centralt centralw centralh
myManageHook = composeAll
      [ className =? "qv2ray" --> doFloat
      , ((className =? "Gimp-2.10") <&&> (title /=? "GNU 图像处理程序")) <||> (title =? "GIMP 启动") --> doFloat
      , className =? "dolphin" --> doFloat
      , className =? "Org.gnome.Nautilus" --> doFloat
      , className =? "Nextcloud" --> doFloat
      , className =? "feh" --> doFloat
      , className =? "kdeconnect.app" --> doFloat
      , className =? "VirtualBox Manager" --> doFloat <+> doShift "8"
      , className =? "Gnome-system-monitor" --> doFloat
      , className =? "kdeconnect-app" --> doFloat
      , className =? "org.jackhuang.hmcl.Launcher" --> doFloat
      , resource =? "desktop_window" --> doIgnore
      , className =? "gnome-screenshot" --> doFloat
      , fullscreenManageHook
      , title /=? "alacritty-drop" --> placeHook (withGaps (100,100,100,100) (underMouse (0,0)))
      ]
    <+> namedScratchpadManageHook myScratchPads
-- toggleHDMI = do
  -- screencount <- LIS.countScreens
  -- if screencount > 1
   -- then spawn "xrandr --output eDP-1-1 --primary --mode 1920x1080 --pos 0x1080 --output HDMI-0 --mode 1920x1080 --pos 0x0 --rotate normal"
   -- else spawn "xrandr --output eDP-1-1 --primary --mode 1920x1080 --pos 0x1080"
myStartupHook = do
  spawnOnce "feh --bg-fill ~/图片/大鱼海棠16.png"
  spawn "killall picom; picom --dbus --experimental-backend"
  spawnOnce "qv2ray"
  spawnOnce "ibus-daemon -x -r -R"
  spawnOnce "nm-applet"
  spawn "killall eww; eww open-many bar"
  -- spawnOnce "sleep 1 ; amixer set Master 0%"
  spawn "gpaste-client start"
  spawn "xrandr --output eDP-1-1 --primary --mode 1920x1080 --pos 0x1080 --output HDMI-0 --mode 1920x1080 --pos 0x0 --rotate normal"
  -- toggleHDMI
  spawn "xhost +"
  -- spawnOnce "light-locker --lock-on-suspend &"
myLayout =  avoidStruts $ smartBorders
  (
  spiralgaps
  ||| threeCol
  -- ||| bspgaps
  -- ||| tiledgaps
  -- ||| StateFull
  ||| (noBorders $ fullscreenFull StateFull)
  ||| (myThinGaps Grid)
  -- ||| simpleTabbed
  -- ||| Mirror tiledgaps
  )
 where
  threeCol   = myGaps $ ThreeCol 1 (3/100) (1/2)
  tiledgaps  = myGaps $ Tall nmaster delta ratio
  -- window number in master pane
  nmaster    = 1
  -- percent of screen to increment by when resizing panes
  delta      = 2 / 100
  -- default proportion of screen occupied by master pane
  ratio      = 0.618

  bspgaps    = myGaps emptyBSP
  spiralgaps = myGaps $ spiral (6 / 7)
myModMask :: KeyMask
myModMask = mod4Mask
myTerminal :: String
myTerminal = "alacritty"
myFont :: String
myFont = "xft:Noto Sans:size=10"
myEmojiFont :: String
myEmojiFont = "xft:Apple Color Emoji:size=11"
myWorkspaces :: [String]
myWorkspaces =
  [ "1"
  , "2"
  , "3"
  , "4"
  , "5"
  , "6"
  , "7"
  , "8"
  , "9"
  ]
myBorderWidth :: Dimension
myBorderWidth = 2
myPromptHeight :: Dimension
myPromptHeight = 30
myNormalBorderColor :: String
-- myNormalBorderColor = "#d6778c"
myNormalBorderColor = "#D35D6E"
myFocusedBorderColor :: String
myFocusedBorderColor = "#ffffff"
myGaps = spacingRaw False (Border 3 3 3 3) True (Border 3 3 3 3) True
myThinGaps = spacingRaw False (Border 3 3 3 3) True (Border 3 3 3 3) True
myLogHook dbus = dynamicLogWithPP (barHook dbus) <+> historyHook <+> updatePointer (0.5, 0.5) (0, 0)
myEventHook = handleEventHook def <+> fullscreenEventHook
getActiveLayoutDescription :: X String
getActiveLayoutDescription = do
  workspaces <- gets windowset
  return $ description . W.layout . W.workspace . W.current $ workspaces
mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = (D.signal opath iname mname)
      body   = [D.toVariant $ UTF8.decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

barHook :: D.Client -> PP
barHook dbus =
  let symbol w | w == "1" = "1:\xf269"
               | w == "2" = "2:\xf120"
               | w == "3" = "3:\xe7a8"
               | w == "4" = "4:\xf48a"
               | w == "5" = "5:\xf126"
               | w == "6" = "6:\xf121"
               | w == "7" = "7:\xf313"
               | w == "8" = "8:\xf308"
               | w == "9" = "9:\xf872"
               | otherwise = w
      wrapper c s | s /= "NSP" = wrap ("(ws :type \"" <> c <> "\" :text \"") "\")" $ symbol s
                  | otherwise  = mempty
      wrapper_layout s = wrap ("(layout :text \"") "\")" s
      layout_map x | x == "Spacing Spiral" = wrapper_layout "\xfa6d"
                   | x == "Spacing ThreeCol" = wrapper_layout "\xfc26"
                   | x == "StateFull" = wrapper_layout "\xf792"
                   | x == "Mirror Spacing Grid" = wrapper_layout "\xfa6f"
                   | x == "Spacing Grid" = wrapper_layout "\xfa6f"
                   | otherwise             = wrapper_layout x
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper "highlight-workspace"
          , ppVisible         = wrapper "normal-workspace"
          , ppUrgent          = wrapper "normal-workspace"
          , ppHidden          = wrapper "normal-workspace"
          , ppHiddenNoWindows = wrapper "normal-workspace"
          , ppLayout          = layout_map
          , ppTitle           = mempty
          , ppSep             = ""
          , ppOrder           = reverse
          }
-- barHook :: D.Client -> PP
-- barHook dbus =
  -- let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  -- | otherwise  = mempty
      -- blue   = "#2E9AFE"
      -- gray   = "#7F7F7F"
      -- orange = "#ea4300"
      -- purple = "#9058c7"
      -- red    = "#ff0000"
  -- in  def { ppOutput          = dbusOutput dbus
          -- , ppCurrent         = wrapper blue
          -- , ppVisible         = wrapper "#ffffff"
          -- , ppUrgent          = wrapper orange
          -- , ppHidden          = wrapper "#eeeeee"
          -- , ppHiddenNoWindows = wrapper "#cccccc"
          -- , ppTitle           = mempty
          -- }

main :: IO ()
main = mkDbusClient >>= main'
main' :: D.Client -> IO ()
main' dbus = do
  -- h <- spawnPipe "xmobar --recompile ~/.xmonad/xmobar.hs"
  xmonad $ fullscreenSupport $ docks $ withNavigation2DConfig def $ ewmh
    def { handleEventHook = handleEventHook def <+> fullscreenEventHook }
      {
      -- simple stuff
        terminal           = (myTerminal++" -e tmux")
      , focusFollowsMouse  = myFocusFollowsMouse
      , clickJustFocuses   = myClickJustFocuses
      , borderWidth        = myBorderWidth
      , modMask            = myModMask
      , workspaces         = myWorkspaces
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      -- key bindings
      -- , keys               = myKeys
      , mouseBindings      = myMouseBindings
      -- hooks, layouts
      , layoutHook         = myLayout
      , manageHook         = myManageHook
      , handleEventHook    = myEventHook
      , logHook            = (myLogHook dbus)
      , startupHook        = myStartupHook
      -- clientMask
      -- rootMask
      -- handleExtraArgs
      } `additionalKeysP` myKeys
