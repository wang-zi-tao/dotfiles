-- This file is generated from "README.org"
import           Control.Arrow                        (first)
import qualified Data.Map                             as M
import           System.Exit                          (exitSuccess)
import           XMonad                               hiding ((|||))
import           XMonad.Actions.Navigation2D          (Direction2D (D, L, R, U),
                                                       windowGo,
                                                       windowSwap,
                                                       withNavigation2DConfig)

import qualified Codec.Binary.UTF8.String             as UTF8
import qualified DBus                                 as D
import qualified DBus.Client                          as D
import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Actions.GridSelect
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.ShowText
import           XMonad.Config.Kde
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.EwmhDesktops            (ewmh,
                                                       fullscreenEventHook)
import           XMonad.Hooks.ManageDocks             (avoidStruts, docks)
import           XMonad.Hooks.ManageHelpers           (doFullFloat,
                                                       isFullscreen)
import           XMonad.Layout.AutoMaster
import           XMonad.Layout.BinarySpacePartition   (Rotate (Rotate),
                                                       Swap (Swap), emptyBSP)
import           XMonad.Layout.Fullscreen             (fullscreenFull,
                                                       fullscreenSupport)
import           XMonad.Layout.Grid                   (Grid (..))
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
import           XMonad.Util.Run                      (hPutStrLn, spawnPipe)
import           XMonad.Util.SpawnOnce                (spawnOnce)
-- import XMonad.Layout.WindowNavigation
-- import XMonad.Actions.GroupNavigation
myLayout =  avoidStruts $ smartBorders
  (
  spiralgaps
  ||| threeCol
  -- ||| bspgaps
  -- ||| tiledgaps
  ||| StateFull
  ||| myThinGaps Grid
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
  [ "1:\xf269"
  , "2:\xf120"
  , "3:\xe7a8"
  , "4:\xf48a"
  , "5:\xf126"
  , "6:\xf121"
  , "7:\xf313"
  , "8:\xf308"
  , "9:\xf872"
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
myGaps = spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
myThinGaps = spacingRaw False (Border 2 2 2 2) True (Border 2 2 2 2) True
myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch myNavigation)
         ,((0,xK_Left)  , move (-1,0)  >> myNavigation)
         ,((0,xK_h)     , move (-1,0)  >> myNavigation)
         ,((0,xK_Right) , move (1,0)   >> myNavigation)
         ,((0,xK_l)     , move (1,0)   >> myNavigation)
         ,((0,xK_Down)  , move (0,1)   >> myNavigation)
         ,((0,xK_j)     , move (0,1)   >> myNavigation)
         ,((0,xK_Up)    , move (0,-1)  >> myNavigation)
         ,((0,xK_k)     , move (0,-1) >> myNavigation)
         ,((0,xK_u)     , move (-1,1) >> myNavigation)
         ,((0,xK_i)     , move (1,1)  >> myNavigation)
         ,((0,xK_n)     , move (-1,-1)  >> myNavigation)
         ,((0,xK_m)     , move (1,-1)  >> myNavigation)
         ,((0,xK_space) , setPos (0,0) >> myNavigation)
         ]
       -- The navigation handler ignores unknown key symbols
       navDefaultHandler = const myNavigation
myGsconfig colorizer = (buildDefaultGSConfig colorizer)
        {gs_cellheight = 200
        ,gs_cellpadding = 10
        ,gs_cellwidth = 200
        ,gs_navigate = myNavigation
        ,gs_font = "xft:SpaceMono Nerd Font-11"
        }
myWinColorizer = colorRangeFromClassName
                     black            -- lowest inactive bg
                     (0x80,0x80,0xFF) -- highest inactive bg
                     black            -- active bg
                     white            -- inactive fg
                     (0x00,0x00,0xFF)            -- active fg
  where black = minBound
        white = maxBound

myKeys :: [(String, X ())]
myKeys =
  [
    ("M-<Return>", spawn myTerminal)
  , ("M-S-c", kill) -- Close focused application
  , ("M-o", spawn "light-locker-command -l") -- lock screen
  , ("M-S-q", confirmPrompt myXPConfig "exit" $ io exitSuccess) -- prompt to kill xmonad
  , ("M-q", spawn "xmonad --restart") 

  , ("M-r", spawn $ myTerminal ++ " -e ranger")
  , ("M-b", spawn "firefox")
  , ("M-p", spawn "gpaste-client ui")
  , ("M-e", spawn "~/.emacs_anywhere/bin/run")
  , ("M-v", spawn "qv2ray")
  , ("M-n", spawn "nautilus")
  , ("M-S-z", spawn "alacritty -e zsh")

  , ("M-x", namedScratchpadAction myScratchPads "terminal")
  , ("M-z", spawn "~/.config/eww/scripts/trigger")

  -- , ("M-d", shellPrompt myXPConfig)
  ,("M-d", spawn "rofi -combi-modi window,run,drun -show combi -modi combi")
  , ("C-M-d", spawn "rofi -show run")
  , ("C-M-l", spawn "light-locker")
  , ("M-<Esc>", nextMatch Forward isOnAnyVisibleWS)
  , ("M-<Tab>", nextMatch History (return True))


  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")

  , ("<Print>", spawn "scrot '截图_%Y%m%d_%H%M%S.png' -e 'mkdir -p ~/图片 && mv $f ~/图片 && xclip -selection clipboard -t image/png -i ~/图片/`ls -1 -t ~/图片 | head -1`'")
  , ("S-<Print>", spawn "scrot -s '截图_%Y%m%d_%H%M%S.png' -e 'mkdir -p ~/图片 && mv $f ~/图片 && xclip -selection clipboard -t image/png -i ~/图片/`ls -1 -t ~/图片 | head -1`' # Area selection")

  , ("M-<Space>", sendMessage NextLayout) -- Change to next layout in order

  -- , ("M-t", sendMessage $ JumpToLayout "Spacing Tall")
  -- , ("M-f", sendMessage $ JumpToLayout "Full")
  -- , ("M-m", sendMessage $ JumpToLayout "Mirror Spacing Tall")
  -- , ("M-n", sendMessage $ JumpToLayout "Spacing BSP")
  -- , ("M-s", sendMessage $ JumpToLayout "Spacing Spiral")
  , ("M-f", goToSelected $ myGsconfig defaultColorizer)

  , ("M-S-t", withFocused $ windows . W.sink) -- unfloat window

  , ("M-<Page_Down>", moveTo Next NonEmptyWS)
  , ("M-<Page_Up>", moveTo Prev NonEmptyWS)
  , ("M-S-<Page_Down>", shiftToNext >> nextWS)
  , ("M-S-<Page_Up>", shiftToPrev >> prevWS)

  , ("M-h", windowGo L False)
  , ("M-l", windowGo R False)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-g", windows W.focusMaster)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-S-g", windows W.swapMaster)

  , ("M-S-h", sendMessage Shrink)
  , ("M-S-l", sendMessage Expand)

   -- Directional navigation of windows
   , ("M-<R>", windowGo R False)
   , ("M-<L>", windowGo L False)
   , ("M-<U>", windowGo U False)
   , ("M-<D>", windowGo D False)

   -- Swap adjacent windows
   , ("M-S-<R>", windowSwap R False)
   , ("M-S-<L>", windowSwap L False)
   , ("M-S-<U>", windowSwap U False)
   , ("M-S-<D>", windowSwap D False)

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

  spawnTerm  = myTerminal ++ " --title=terminalScratchpad --config-file=${HOME}/.config/alacritty/alacritty-drop.yml "
  findTerm   = title =? "terminalScratchpad"
  manageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.05
    l = (1 - w) / 2

  managevifm =
    customFloating $ W.RationalRect centrall centralt centralw centralh
myManageHook =
  composeAll
      [ title =? "GIMP 启动" --> doFloat
      , className =? "qv2ray" --> doFloat
      , className =? "Org.gnome.Nautilus" --> doFloat
      , className =? "feh" --> doFloat
      , className =? "VirtualBox Manager" --> doFloat
      , className =? "Gnome-system-monitor" --> doFloat
      , className =? "kdeconnect-app" --> doFloat
      , resource =? "desktop_window" --> doIgnore
      , isFullscreen --> doFullFloat
      ]
    <+> namedScratchpadManageHook myScratchPads
myStartupHook = do
  spawnOnce "feh --bg-fill ~/图片/大鱼海棠16.png"
  spawn "killall picom; picom --dbus --experimental-backend"
  spawnOnce "qv2ray"
  spawnOnce "sleep 1 && ibus-daemon --xim"
  spawnOnce "nm-applet"
  -- spawnOnce "polybar main"
  -- spawn "killall polybar; polybar left & polybar center & polybar right &"
  spawn "killall eww; eww open-many bar bar1"
  spawnOnce "kdeconnect-indicator"
  spawnOnce "sleep 1 ; amixer set Master 0%"
  spawn "gpaste-client start"
  spawn "xrandr --output eDP-1-1 --primary --mode 1920x1080 --pos 0x1080 --rotate normal --output HDMI-0 --mode 1920x1080 --pos 0x0 --rotate normal"
  -- setDefaultCursor xC_left_ptr
  -- spawn Japanese IME
  -- spawnOnce "fcitx -d &"
  -- start screen locker
  -- spawnOnce "light-locker --lock-on-suspend &"
  -- window animation
  -- spawnOnce "flashfocus &"
myLogHook h = dynamicLogWithPP xmobarPP
  { ppOutput          = hPutStrLn h
  , ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort def) -- hide nsp
  , ppCurrent         = xmobarColor "#03befc" "" -- Current workspace
  , ppVisible         = xmobarColor "#03fcfc" ""
  , ppHidden          = xmobarColor "#bfffff" ""
  , ppHiddenNoWindows = xmobarColor "#FFFFFF" ""
  , ppLayout          = xmobarColor "#82aaff" ""
  , ppSep             = " | "
  , ppTitle           = mempty
  }
myPolybarLogHook dbus = dynamicLogWithPP (polybarHook dbus) <+> historyHook
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

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("(label :style \"color:" <> c <> ";\" :text \"") "\")" s
                  | otherwise  = mempty
      blue   = "#2E9AFE"
      gray   = "#7F7F7F"
      orange = "#ea4300"
      purple = "#9058c7"
      red    = "#ff0000"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper blue
          , ppVisible         = wrapper "#ffffff"
          , ppUrgent          = wrapper orange
          , ppHidden          = wrapper "#eeeeee"
          , ppHiddenNoWindows = wrapper "#cccccc"
          , ppLayout          = wrapper "#ffffff"
          , ppTitle           = mempty
          , ppSep             = ""
          , ppOrder           = \(ws:_:t:_) -> [ws]
          }
-- polybarHook :: D.Client -> PP
-- polybarHook dbus =
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
  xmonad $ docks $ withNavigation2DConfig def $ ewmh
    def { handleEventHook = handleEventHook def <+> fullscreenEventHook }
      {
      -- simple stuff
        terminal           = myTerminal
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
      , logHook            = (myPolybarLogHook dbus)
      , startupHook        = myStartupHook
      -- clientMask
      -- rootMask
      -- handleExtraArgs
      } `additionalKeysP` myKeys
