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
import           XMonad.Actions.Search
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
import XMonad.Hooks.WallpaperSetter

-- | q ^? x. if the result of x 'isPrefixOf' q, return True
(<^?) :: (Eq a, Functor m) => m [a] -> [a] -> m Bool
q <^? x = fmap (x `isPrefixOf`) q
myWorkspaces         = [ "1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , "9" ]
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
            then spawn (myTerminal ++ " --title=alacritty-workspace-" ++ w ++ " -e remote-shell.sh 'workspace-" ++ w ++ "'")
            else windows $ (W.focusWindow $ alacritty_windows_workspace !! 0)
    )
  , ("M-S-<Return>", spawn "alacritty -e tmux")
  , ("M-S-c", kill)
  , ("M-c", kill)
  , ("M-Delete", kill)
  , ("M-o", spawn "i3lock-fancy && systemctl suspend") -- lock screen
  , ("C-M-o", spawn "i3lock-fancy")
  , ("C-M-l", spawn "i3lock-fancy")
  -- , ("M-S-q", confirmPrompt myXPConfig "exit" $ io exitSuccess) -- prompt to kill xmonad
  , ("M-q", spawn "xmonad --restart")

  , ("M-b", spawn "firefox")
  , ("M-p", spawn "gpaste-client ui")
  , ("M-i", spawn "kdeconnect-app")
  -- , ("M-v", namedScratchpadAction myScratchPads "qv2ray")
  , ("M-v", spawn "qv2ray")
  , ("M-n", namedScratchpadAction myScratchPads "nautilus")
  , ("M-S-n", spawn "dolphin")
  , ("M-S-z", spawn "alacritty -e zsh")

  , ("M-x", namedScratchpadAction myScratchPads "terminal")
  , ("M-z", spawn "~/.config/eww/scripts/trigger.sh")

  , ("M-d", spawn "rofi -combi-modi window,drun -show combi -modi combi -theme ~/.config/rofi/apps.css")
  , ("M-f", spawn "rofi -combi-modi window -show combi -modi combi -theme ~/.config/rofi/apps.css")
  , ("M-r", spawn "rofi -combi-modi run -show combi -modi combi -theme ~/.config/rofi/apps.css")
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
  where
    getActiveLayoutDescription = do
      workspaces <- gets windowset
      return $ description . W.layout . W.workspace . W.current $ workspaces

myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList
  [ ((modm, button1) , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster )
  , ((modm, button2) , \w -> focus w >> kill )
  , ((modm, button3) , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster )
  ]
myScratchPads = [ NS "terminal" spawnTerm    findTerm   manageTerm
                , NS "nautilus" "nautilus" (className =? "Org.gnome.Nautilus") longManageTerm
                , NS "qv2ray" "q2ray" (title =? "Qv2ray") smallManageTerm
                ]
 where
  spawnTerm  = myTerminal ++ " --title=alacritty-drop --config-file=${HOME}/.config/alacritty/alacritty-drop.yml "
  findTerm   = title =? "alacritty-drop"
  manageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.905
    w = 0.9
    t = 0.05
    l = (1 - w) / 2
  smallManageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.6
    w = 0.6
    t = (1 - h) / 2
    l = (1 - w) / 2
  longManageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.5
    w = 0.7
    t = (1 - h) / 2
    l = (1 - w) / 2

myManageHook = composeAll
      [ fullscreenManageHook
      , className =? "qv2ray" --> doFloat
      , ((className =? "Gimp") <&&> (title /=? "GNU 图像处理程序")) <||> (title =? "GIMP 启动") --> doFloat
      , className =? "dolphin" --> doFloat
      -- , className =? "Org.gnome.Nautilus" --> doFloat
      , className =? "Nextcloud" --> doFloat
      , className =? "feh" --> doFloat
      , className =? "kdeconnect.app" --> doFloat
      , className =? "VirtualBox Manager" --> doFloat <+> doShift "8"
      , className =? "alacritty-workspace-2" --> doShift "2"
      , className =? "Gnome-system-monitor" --> doFloat
      , className =? "kdeconnect-app" --> doFloat
      , className =? "org.jackhuang.hmcl.Launcher" --> doFloat
      , resource =? "desktop_window" --> doIgnore
      , className =? "icalingua" --> doShift "6"
      , (className =? "gnome-screenshot") <||> (className =? "Gnome-screenshot") --> doFloat
      , (className =? "Firefox" )  --> doShift "1"
      , title /=? "alacritty-drop" --> placeHook (withGaps (100,100,100,100) (underMouse (0,0)))
      , title =? "alacritty-workspace-1" --> doShift "1"
      , title =? "alacritty-workspace-2" --> doShift "2"
      , title =? "alacritty-workspace-3" --> doShift "3"
      , title =? "alacritty-workspace-4" --> doShift "4"
      , title =? "alacritty-workspace-5" --> doShift "5"
      , title =? "alacritty-workspace-6" --> doShift "6"
      , title =? "alacritty-workspace-7" --> doShift "7"
      , title =? "alacritty-workspace-8" --> doShift "8"
      , title =? "alacritty-workspace-9" --> doShift "9"
      ] <+> namedScratchpadManageHook myScratchPads
-- toggleHDMI = do
  -- screencount <- LIS.countScreens
  -- if screencount > 1
   -- then spawn "xrandr --output eDP-1-1 --primary --mode 1920x1080 --pos 0x1080 --output HDMI-0 --mode 1920x1080 --pos 0x0 --rotate normal"
   -- else spawn "xrandr --output eDP-1-1 --primary --mode 1920x1080 --pos 0x1080"
myStartupHook = do
  spawn "killall picom; picom --dbus --experimental-backend"
  -- spawnOnce "qv2ray"
  spawnOnce "ibus-daemon -x -r -R"
  spawnOnce "sleep 4;nextcloud"
  spawn "killall eww; eww open-many bar"
  spawn "killall polybar;sleep 4; polybar icons"
  spawn "gpaste-client start"
  spawn "xrandr --output eDP-1-1 --primary --mode 1920x1080 --pos 0x1080 --output HDMI-0 --mode 1920x1080 --pos 0x0 --rotate normal"
  spawn "xhost +"
  spawnOnce "alacritty --title=alacritty-workspace-2 -e tmuxinator s workspace-2"
  spawnOnce "firefox"
  -- spawnOnce "icalingua"

myLayout =  avoidStruts $ smartBorders
  ( spiralgaps ||| threeCol ||| (noBorders $ fullscreenFull StateFull) ||| (myThinGaps Grid) )
 where
  myThinGaps = spacingRaw False (Border 3 3 3 3) True (Border 3 3 3 3) True
  myGaps = spacingRaw False (Border 3 3 3 3) True (Border 3 3 3 3) True
  threeCol   = myGaps $ ThreeCol 1 (3/100) (1/3)
  tiledgaps  = myGaps $ Tall 1 (2/100) (0.618)
  bspgaps    = myGaps emptyBSP
  spiralgaps = myGaps $ spiral (6 / 7)
myTerminal :: String
myTerminal = "alacritty"
myLogHook dbus = dynamicLogWithPP (barHook dbus) <+> historyHook <+> wallpapersHook -- <+> updatePointer (0.5, 0.5) (0, 0)
wallpapersHook = wallpaperSetter defWallpaperConf {
     wallpapers = defWPNames myWorkspaces
               <> WallpaperList [
                 ("1",WallpaperFix "/home/wangzi/.xmonad/wallpapers/大鱼海棠16.jpg")
                ,("2",WallpaperFix "/home/wangzi/.xmonad/wallpapers/大鱼海棠9.jpg")
                ,("3",WallpaperFix "/home/wangzi/.xmonad/wallpapers/大鱼海棠14.jpg")
                ,("4",WallpaperFix "/home/wangzi/.xmonad/wallpapers/大鱼海棠15.jpg")
                ,("5",WallpaperFix "/home/wangzi/.xmonad/wallpapers/大鱼海棠8.jpg")
                ,("6",WallpaperFix "/home/wangzi/.xmonad/wallpapers/大鱼海棠6.jpg")
                ,("7",WallpaperFix "/home/wangzi/.xmonad/wallpapers/大鱼海棠3.jpg")
                ,("8",WallpaperFix "/home/wangzi/.xmonad/wallpapers/大鱼海棠1.jpg")
                ,("9",WallpaperFix "/home/wangzi/.xmonad/wallpapers/大鱼海棠.jpg")
               ]
  }
mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
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
  let symbol w | w == "1" = "\xf269"
               | w == "2" = "\xf120"
               | w == "3" = "\xe7a8"
               | w == "4" = "\xf48a"
               | w == "5" = "\xf126"
               | w == "6" = "\xfb04"
               | w == "7" = "\xf313"
               | w == "8" = "\xf308"
               | w == "9" = "\xf872"
               | otherwise = w
      wrapper c s | s /= "NSP" = "(ws :type \"" ++ c ++ "\" :text \"" ++ (symbol s) ++ "\" :name \"" ++ s ++ "\")"
                  | otherwise  = mempty
      wrapper_layout s = "(layout :text \"" ++ s ++ "\")"
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
main :: IO ()
main = do
  dbus <- mkDbusClient
  xmonad $ fullscreenSupport $ docks $ withNavigation2DConfig def $ ewmh
    def { handleEventHook = handleEventHook def <+> fullscreenEventHook }
      {
      -- simple stuff
        terminal           = (myTerminal++" -e tmux")
      , focusFollowsMouse  = True
      , clickJustFocuses   = True
      , borderWidth        = 2
      , modMask            = mod4Mask
      , workspaces         = myWorkspaces
      , normalBorderColor  = "#ffffff" -- "#d6778c"
      , focusedBorderColor = "#5a9dd8"
      -- key bindings
      -- , keys               = myKeys
      , mouseBindings      = myMouseBindings
      -- hooks, layouts
      , layoutHook         = myLayout
      , manageHook         = myManageHook
      , handleEventHook    = handleEventHook def <+> fullscreenEventHook
      , logHook            = (myLogHook dbus)
      , startupHook        = myStartupHook
      -- clientMask
      -- rootMask
      -- handleExtraArgs
      } `additionalKeysP` myKeys
