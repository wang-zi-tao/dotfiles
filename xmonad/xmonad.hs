-- This file is generated from "README.org"
import           Control.Arrow                      (first)
import qualified Data.Map                           as M
import           System.Exit                        (exitSuccess)
import           XMonad                             hiding ((|||))
import           XMonad.Actions.Navigation2D        (Direction2D (L, R),
                                                     windowGo,
                                                     withNavigation2DConfig)

import qualified Codec.Binary.UTF8.String           as UTF8
import qualified DBus                               as D
import qualified DBus.Client                        as D
import           XMonad.Actions.GridSelect
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.ShowText
import           XMonad.Config.Kde
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops          (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks           (avoidStruts, docks)
import           XMonad.Hooks.ManageHelpers         (doFullFloat, isFullscreen)
import           XMonad.Layout.AutoMaster
import           XMonad.Layout.BinarySpacePartition (Rotate (Rotate),
                                                     Swap (Swap), emptyBSP)
import           XMonad.Layout.Fullscreen           (fullscreenFull,
                                                     fullscreenSupport)
import           XMonad.Layout.Grid                 (Grid (..))
import           XMonad.Layout.LayoutCombinators    (JumpToLayout (JumpToLayout),
                                                     (|||))
import           XMonad.Layout.NoBorders            (noBorders, smartBorders)
import           XMonad.Layout.ResizableTile        (ResizableTall (..))
import           XMonad.Layout.Spacing              (Border (Border),
                                                     spacingRaw)
import           XMonad.Layout.Spiral               (spiral)
import           XMonad.Layout.StateFull
import           XMonad.Layout.Tabbed               (simpleTabbed)
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.TwoPane              (TwoPane (..))
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt        (confirmPrompt)
import           XMonad.Prompt.FuzzyMatch           (fuzzyMatch)
import           XMonad.Prompt.Man                  (manPrompt)
import           XMonad.Prompt.Shell                (shellPrompt)
import           XMonad.Prompt.Unicode              (mkUnicodePrompt)
import qualified XMonad.StackSet                    as W
import           XMonad.Util.Cursor                 (setDefaultCursor)
import           XMonad.Util.EZConfig               (additionalKeysP)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                    (hPutStrLn, spawnPipe)
import           XMonad.Util.SpawnOnce              (spawnOnce)
-- import XMonad.Layout.WindowNavigation
myLayout =  avoidStruts $ smartBorders
  (
  spiralgaps
  ||| threeCol
  ||| bspgaps
  ||| tiledgaps
  ||| StateFull
  -- ||| TwoPane 15/100 55/100
  -- ||| Mirror (Tall 1 10/100 60/100)
  ||| Grid
  ||| simpleTabbed
  ||| Mirror tiledgaps
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
  [ "1:\xe7c5"
  , "2:\xf269"
  , "3:\xf489"
  , "4:\xf795"
  , "5:\xf126"
  , "6:\xf121"
  , "7:\xf313"
  , "8:\xfcb3"
  , "9:\xf872"
  ]
myBorderWidth :: Dimension
myBorderWidth = 1
myPromptHeight :: Dimension
myPromptHeight = 30
myNormalBorderColor :: String
myNormalBorderColor = "#2b2a3e"
myFocusedBorderColor :: String
myFocusedBorderColor = "#c792ea"
myGaps = spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
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
        {gs_cellheight = 50
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
    ("M-t", spawn "firefox" )
  , ("M-<Return>", spawn myTerminal)
  , ("M-S-c", kill) -- Close focused application
  , ("M-o", spawn "light-locker-command -l") -- lock screen
  , ("M-S-q", confirmPrompt myXPConfig "exit" $ io exitSuccess) -- prompt to kill xmonad
  , ("M-q", spawn "xmonad --recompile; xmonad --restart") -- Recompile and restart xmonad

  , ("M-v", spawn $ myTerminal ++ " -e nvim")
  , ("M-z", spawn "emacsclient -c -a emacs ~/")
  , ("M-w", spawn "emacsclient -c -a emacs")
  , ("M-b", spawn "firefox")
  , ("M-p", spawn "gpaste-client ui")
  , ("M-e", spawn "~/.emacs_anywhere/bin/run")

  , ("M-x", namedScratchpadAction myScratchPads "terminal")
  -- , ("M-C-s", namedScratchpadAction myScratchPads "mixer")
  -- , ("M-C-h", namedScratchpadAction myScratchPads "bottom")
  -- , ("M-C-n", namedScratchpadAction myScratchPads "vifm")

  -- , ("M-d", shellPrompt myXPConfig)
  ,("M-d", spawn "rofi -combi-modi window,run,drun -show combi -modi combi")
  , ("C-M-d", spawn "rofi -show run")
  , ("M-<Esc>", nextMatch Forward isOnAnyVisibleWS)


  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")

  , ("M-<Space>", sendMessage NextLayout) -- Change to next layout in order

  -- , ("M-t", sendMessage $ JumpToLayout "Spacing Tall")
  -- , ("M-f", sendMessage $ JumpToLayout "Full")
  -- , ("M-m", sendMessage $ JumpToLayout "Mirror Spacing Tall")
  -- , ("M-n", sendMessage $ JumpToLayout "Spacing BSP")
  -- , ("M-s", sendMessage $ JumpToLayout "Spacing Spiral")
  , ("M-f", goToSelected $ myGsconfig defaultColorizer)


  , ("M-S-t", withFocused $ windows . W.sink) -- unfloat window

  , ("M-r", refresh)

  -- focus horizontally like i3wm
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
                 , borderColor       = "#2b2a3e"
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
                      , borderColor       = "#2b2a3e"
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
myClickJustFocuses = False

myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
  [ ( (modm, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )

    -- mod-button2, Raise the window to the top of the stack
  , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
  , ( (modm, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]
myScratchPads =
  [ NS "terminal" spawnTerm   findTerm   manageTerm
  , NS "mixer"    spawnMixer  findMixer  manageMixer
  , NS "bottom"   spawnbottom findbottom managebottom
  , NS "vifm"     spawnvifm   findvifm   managevifm
  ]
 where
  centralh   = 0.9
  centralw   = 0.9
  centralt   = 0.95 - centralh
  centrall   = 0.95 - centralw

  spawnTerm  = myTerminal ++ " --title=terminalScratchpad"
  findTerm   = title =? "terminalScratchpad"
  manageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.6
    w = 0.9
    t = 0.05
    l = (1 - w) / 2

  spawnMixer = myTerminal ++ " --title=mixerScratchpad" ++ " -e ncpamixer"
  findMixer  = title =? "mixerScratchpad"
  manageMixer =
    customFloating $ W.RationalRect centrall centralt centralw centralh

  spawnbottom = myTerminal ++ " --title=bottomScratchpad" ++ " -e btm"
  findbottom  = title =? "bottomScratchpad"
  managebottom =
    customFloating $ W.RationalRect centrall centralt centralw centralh

  spawnvifm =
    myTerminal ++ " --title=vifmScratchpad" ++ " -e bash -c 'vifmrun'"
  findvifm = title =? "vifmScratchpad"
  managevifm =
    customFloating $ W.RationalRect centrall centralt centralw centralh
myManageHook =
  composeAll
      [ className =? "Gimp" --> doFloat
      , resource =? "desktop_window" --> doIgnore
      , isFullscreen --> doFullFloat
      ]
    <+> namedScratchpadManageHook myScratchPads
myStartupHook = do
  spawnOnce "feh --bg-fill ~/图片/大鱼海棠16.png"
  spawnOnce "picom"
  spawnOnce "qv2ray"
  spawnOnce "sleep 1 && ibus-daemon --xim"
  spawnOnce "nm-applet"
  spawnOnce "polybar main"
  spawnOnce "gnome-power-manager"
  spawnOnce "gnome-volume-control-applet"
  spawnOnce "kdeconnect-indicator"
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
myPolybarLogHook dbus = dynamicLogWithPP (polybarHook dbus)
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
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      blue   = "#2E9AFE"
      gray   = "#7F7F7F"
      orange = "#ea4300"
      purple = "#9058c7"
      red    = "#722222"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper blue
          , ppVisible         = wrapper gray
          , ppUrgent          = wrapper orange
          , ppHidden          = wrapper gray
          , ppHiddenNoWindows = wrapper red
          , ppTitle           = shorten 100 . wrapper purple
          }

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
      } `additionalKeysP` myKeys
