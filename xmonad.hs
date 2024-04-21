{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import           Control.Monad                       (unless, when, void)
import           Data.Bits                           (testBit)
import           Data.Foldable                       (find)
import qualified Data.Map                            as M
import           Data.Maybe                          (fromJust)
import           Data.Semigroup
import           Foreign.C                           (CInt)
import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Xinerama               (getScreenInfo)
import           Prelude                             hiding (log)
import           System.Exit
import           XMonad
import qualified XMonad.StackSet                     as W

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition         (Focus (Newer),
                                                      Position (End, Master),
                                                      insertPosition)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers          (doCenterFloat, doSink,
                                                      isDialog)
import           XMonad.Hooks.OnPropertyChange
import           XMonad.Hooks.RefocusLast            (isFloat)
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.WindowSwallowing

import           XMonad.Layout.Decoration            (ModifiedLayout)
import           XMonad.Layout.DraggingVisualizer    (draggingVisualizer)
import           XMonad.Layout.Dwindle
import           XMonad.Layout.HintedGrid
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.MultiToggle           (EOT (EOT),
                                                      Toggle (Toggle), mkToggle,
                                                      (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import           XMonad.Layout.NoBorders             (hasBorder, smartBorders)
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Spacing               (Border (Border), Spacing,
                                                      spacingRaw)
import           XMonad.Layout.Tabbed

import           Data.List
import qualified Data.List                           as L
import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize       as Flex
import           XMonad.Actions.OnScreen             (onlyOnScreen)
import           XMonad.Actions.TiledWindowDragging
import           XMonad.Actions.UpdatePointer        (updatePointer)
import           XMonad.Actions.Warp                 (warpToScreen)
import           XMonad.Hooks.RefocusLast            (refocusLastLogHook)
import qualified XMonad.Util.ExtensibleState         as XS
import           XMonad.Util.EZConfig                (additionalKeys,
                                                      additionalKeysP)
import           XMonad.Util.Loggers                 (logLayoutOnScreen,
                                                      logTitleOnScreen,
                                                      shortenL, wrapL,
                                                      xmobarColorL)
import           XMonad.Util.NamedScratchpad
import Data.Time
import Data.Time.LocalTime
import Control.Concurrent (forkIO, threadDelay) -- Add this line
import Control.Monad (forever) -- Add this line
myTerminal, myTerminalClass :: [Char]
myTerminal = "termonad"
myTerminalClass = "termonad"

grey1, grey2, grey3, grey4, cyan, orange :: String
grey1  = "#2B2E37"
grey2  = "#555E70"
grey3  = "#697180"
grey4  = "#8691A8"
cyan   = "#8BABF0"
orange = "#C45500"

myWorkspaces :: [[Char]]
myWorkspaces = ["  \58930  ", "\59056  ", "\60362  ", "\62600  ", "\58931  ", "\62221  ", "\989728  ", "\61723  ", "\62065  "]


trayerRestartCommand :: [Char]
trayerRestartCommand = "killall trayer; trayer --monitor 1 --edge top --align right --widthtype request --padding 7 --iconspacing 10 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x2B2E37  --height 29 --distance 5 &"

actionPrefix, actionButton, actionSuffix :: [Char]
actionPrefix = "<action=`xdotool key super+"
actionButton = "` button="
actionSuffix = "</action>"

addActions :: [(String, Int)] -> String -> String
addActions [] ws = ws
addActions (x:xs) ws = addActions xs (actionPrefix ++ k ++ actionButton ++ show b ++ ">" ++ ws ++ actionSuffix)
    where k = fst x
          b = snd x

------------------------------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [
    NS "terminal" "st" (title =? "scratch") (customFloating $ W.RationalRect 0.17 0.15 0.7 0.7)
  , NS "simplecolorpicker" "simplecolorpicker" (className =? "simplecolorpicker") defaultFloating
  , NS "galculator" "galculator" (className =? "Galculator") (customFloating $ W.RationalRect 0.58 0.48 0.2 0.4)
  , NS "btop" (myTerminal ++ " --title btop -e btop") (title =? "btop") (customFloating $ W.RationalRect 0.17 0.15 0.7 0.7)
  , NS "xclicker" "xclicker" (className =? "xclicker") (customFloating $ W.RationalRect 0.435 0.05 0.13 0.21)
  , NS "brightness" "brightness-controller" (title =? "Brightness Controller") defaultFloating
  , NS "discord" "discord" (className =? "Discord") (customFloating $ W.RationalRect 0.17 0.15 0.7 0.7)
  , NS "simplenote" "simplenote --no-sandbox" (className =? "Simplenote") (customFloating $ W.RationalRect 0.76 0.06 0.23 0.91)
  ]

------------------------------------------------------------------------

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)
workspaceOnCurrentScreen :: WSType
workspaceOnCurrentScreen = WSIs $ do
  s <- currentScreen
  return $ \x -> W.tag x /= "NSP" && isOnScreen s x

myAditionalKeys :: [(String, X ())]
myAditionalKeys =
   --volume
    -- apps
  [ ("M-<Return>", spawn myTerminal)
  ,  ("<XF86AudioRaiseVolume>", spawn "sh /home/erel/.config/hypr/scripts/Volume.sh --inc")
            , ("<XF86AudioLowerVolume>", spawn "sh /home/erel/.config/hypr/scripts/Volume.sh --dec")
            , ("<XF86AudioMute>", spawn "sh /home/erel/.config/hypr/scripts/Volume.sh --toggle")
            , ("<XF86AudioMicMute>", spawn "sh /home/erel/.config/hypr/scripts/Volume.sh  --toggle-mic")
--bright

  , ("<XF86MonBrightnessUp>", spawn "lux -a 10%")
  , ("<XF86MonBrightnessDown>", spawn "lux -s 10%") 

  , ("M-v", spawn $ myTerminal ++ " --title Nvim -e nvim")
  , ("M-f", spawn $ myTerminal ++ " --title Ranger -e ranger")
  , ("M-d", spawn "pkill rofi || rofi -show drun -modi drun,filebrowser,run,window")
  , ("M-S-d", spawn "rofi -show run")
  , ("M-p", spawn "passmenu -p pass")
  , ("M-w", spawn "librewolf")
  , ("M-S-w", spawn "librewolf --private-window")
  , ("M-S-t", spawn trayerRestartCommand)
  , ("<Print>", spawn "flameshot gui")
  , ("M-e", spawn $ myTerminal ++ " --title Nvim -e nvim")
  --  , ("M-e", spawn "emacsclient -c -a 'emacs'")
  , ("M-q", kill)

  -- scratchpads
  , ("M-c", namedScratchpadAction myScratchPads "galculator")
  , ("M-y", namedScratchpadAction myScratchPads "btop")
  , ("M-r", namedScratchpadAction myScratchPads "xclicker")
  , ("M-b", namedScratchpadAction myScratchPads "brightness")
  , ("M-S-c", namedScratchpadAction myScratchPads "discord")
  , ("M-S-<Return>", namedScratchpadAction myScratchPads "terminal")
  -- spotify controls
  , ("M-<F9>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
  , ("M-<F11>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
  , ("M-<F12>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

  -- volume controls
  --, ("<F1>", spawn "amixer set Master toggle ")
    --, ("<F2>", spawn "amixer set Master   5%- ")
    --, ("<F3>", spawn "amixer set Master   5%+ ")

  -- window controls
 , ("M-<Up>", windows W.focusUp)
 , ("M-<Down>", windows W.focusDown)
 , ("M-<Left>", windows W.focusMaster)
 , ("M-<Right>", windows W.focusDown)
 , ("M-S-<Right>", windows W.swapDown)
 , ("M-S-<Left>", windows W.swapMaster)
 , ("M-S-<Up>", windows W.swapUp)
 , ("M-C-<Left>", sendMessage Shrink)
 , ("M-C-<Right>", sendMessage Expand)
 , ("M-C-<Down>", sendMessage ShrinkSlave)
 , ("M-C-<Up>", sendMessage ExpandSlave)
 , ("M-<comma>", sendMessage $ IncMasterN  1)
 , ("M-<period>", sendMessage $ IncMasterN (-1))
 , ("M-<Space>", withFocused $ windows . W.sink)


  -- layout controls
  , ("M-a", sendMessage $ Toggle NBFULL)
  , ("M-S-a", sendMessage ToggleStruts)
  , ("M-n", sendMessage NextLayout)
  , ("M-m", spawn "xdotool key super+a && xdotool key super+A")

  -- workspace controls
  , ("M-h", moveTo Prev workspaceOnCurrentScreen)
  , ("M-j", moveTo Next workspaceOnCurrentScreen)
  -- screen controll
  , ("M-o", switchScreen 1)
  , ("M-S-o", shiftNextScreen)

  -- kill / restart xmonad
  , ("M-S-q", io exitSuccess)
  , ("M-S-r", spawn "killall xmobar; xmonad --recompile; xmonad --restart")
  , ("M-b", spawn "xmobar")
  ]


myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
 [((m .|. modm, k), windows $ onCurrentScreen f i)
 | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
 , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
 ]


myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , ((modm .|. shiftMask, button1), dragWindow)
  , ((modm, button2), const kill)
  , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w)
  , ((modm, button4), \_ -> moveTo Prev workspaceOnCurrentScreen)
  , ((modm, button5), \_ -> moveTo Next workspaceOnCurrentScreen)
  ]

------------------------------------------------------------------------

switchScreen :: Int -> X ()
switchScreen d = do s <- screenBy d
                    mws <- screenWorkspace s
                    warpToScreen s 0.618 0.618
                    case mws of
                         Nothing -> return ()
                         Just ws -> windows (W.view ws)

------------------------------------------------------------------------
isDayTime :: IO Bool
isDayTime = do
 currentZonedTime <- getZonedTime
 let currentHour = localTimeOfDay $ zonedTimeToLocalTime currentZonedTime
 return $ not (isNight currentHour)

isNight :: TimeOfDay -> Bool
isNight time = time >= TimeOfDay 20 0 0 || time < TimeOfDay 9 0 0


mySpacing :: Integer -> Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i j = spacingRaw False (Border i i i i) True (Border j j j j) True

myLayoutHook = avoidStruts $ onWorkspaces ["0_9", "1_9"] layoutGrid $ layoutTall ||| layoutTabbed ||| layoutFloat
  where
    layoutTall = mkToggle (NBFULL ?? EOT) . renamed [Replace "tall"] $ draggingVisualizer $ smartBorders $ mySpacing 15 7 $ mouseResizableTile { masterFrac = 0.65, draggerType = FixedDragger 0 30}
    layoutGrid = mkToggle (NBFULL ?? EOT) . renamed [Replace "grid"] $ draggingVisualizer $ smartBorders $ mySpacing 15 7 $ Grid False
    layoutFloat = mkToggle (NBFULL ?? EOT) . renamed [Replace "float"] $ simpleFloat
    layoutTabbed = mkToggle (NBFULL ?? EOT) . renamed [Replace "full"] $ smartBorders $ mySpacing 15 7 $ tabbed shrinkText myTabTheme
    myTabTheme = def
      { fontName            = "xft:Roboto:size=12:bold"
      , activeColor         = grey1
      , inactiveColor       = grey1
      , activeBorderColor   = grey1
      , inactiveBorderColor = grey1
      , activeTextColor     = orange
      , inactiveTextColor   = grey3
      , decoHeight          = 25
      }

------------------------------------------------------------------------

(~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~? x = fmap (x `L.isInfixOf`) q

(/=?) :: Eq a => Query a -> a -> Query Bool
q /=? x = fmap (/= x) q

myManageHook :: ManageHook
myManageHook = composeAll
  [ resource  =? "desktop_window" --> doIgnore
  , isDialog --> doCenterFloat
  , className =? "wow.exe" --> doCenterFloat
  , className =? "battle.net.exe" --> doCenterFloat
  , title =? "Godot" --> doCenterFloat
  , className =? "Blueberry.py" --> doCenterFloat
  , appName =? "blueman-manager" --> doCenterFloat
  , appName =? "pavucontrol" --> doCenterFloat
  , appName =? "nwg-look" --> doCenterFloat
  , title =? myTerminalClass --> insertPosition End Newer
  , insertPosition Master Newer
  ] <+> manageDocks <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------

myHandleEventHook :: Event -> X All
myHandleEventHook = multiScreenFocusHook
                 -- <+> swallowEventHook (className =? myTerminalClass) (return True)
                --  <+> dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> doShift "1_8")

------------------------------------------------------------------------
restartXMonad :: IO ()
restartXMonad = spawn "xmonad --restart"
periodicTasks :: X ()
periodicTasks = do
 liftIO $ void $ forkIO $ forever $ do
    changeWallpaper
    threadDelay (60 * 60 * 1000000) -- Wait for 1 hour
    restartXMonad
changeWallpaper :: IO ()
changeWallpaper = do
 isDay <- isDayTime
 if isDay
    then spawn "feh --bg-tile ~/Pictures/day_wall.jpg"
    else spawn "feh --bg-tile ~/Pictures/night_wall.jpg"

myStartupHook :: X ()
myStartupHook = do
    spawn "ksh /home/erel/.xmonad/start.ksh"
    spawn trayerRestartCommand
    spawn "xsetroot -cursor_name left_ptr &"
    spawn "setxkbmap us"
    spawn "lxsession &"
    liftIO changeWallpaper
    periodicTasks
    modify $ \xstate -> xstate { windowset = onlyOnScreen 1 "1_1" (windowset xstate) }

------------------------------------------------------------------------

newtype MyUpdatePointerActive = MyUpdatePointerActive Bool
instance ExtensionClass MyUpdatePointerActive where
  initialValue = MyUpdatePointerActive True

myUpdatePointer :: (Rational, Rational) -> (Rational, Rational) -> X ()
myUpdatePointer refPos ratio =
  whenX isActive $ do
    dpy <- asks display
    root <- asks theRoot
    (_,_,_,_,_,_,_,m) <- io $ queryPointer dpy root
    unless (testBit m 9 || testBit m 8 || testBit m 10) $ -- unless the mouse is clicking
      updatePointer refPos ratio

  where
    isActive = (\(MyUpdatePointerActive b) -> b) <$> XS.get

------------------------------------------------------------------------

multiScreenFocusHook :: Event -> X All
multiScreenFocusHook MotionEvent { ev_x = x, ev_y = y } = do
  ms <- getScreenForPos x y
  case ms of
    Just cursorScreen -> do
      let cursorScreenID = W.screen cursorScreen
      focussedScreenID <- gets (W.screen . W.current . windowset)
      when (cursorScreenID /= focussedScreenID) (focusWS $ W.tag $ W.workspace cursorScreen)
      return (All True)
    _ -> return (All True)
  where getScreenForPos :: CInt -> CInt
            -> X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
        getScreenForPos x y = do
          ws <- windowset <$> get
          let screens = W.current ws : W.visible ws
              inRects = map (inRect x y . screenRect . W.screenDetail) screens
          return $ fst <$> find snd (zip screens inRects)
        inRect :: CInt -> CInt -> Rectangle -> Bool
        inRect x y rect = let l = fromIntegral (rect_x rect)
                              r = l + fromIntegral (rect_width rect)
                              t = fromIntegral (rect_y rect)
                              b = t + fromIntegral (rect_height rect)
                           in x >= l && x < r && y >= t && y < b
        focusWS :: WorkspaceId -> X ()
        focusWS ids = windows (W.view ids)
multiScreenFocusHook _ = return (All True)

------------------------------------------------------------------------

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..]

clickable :: [Char] -> [Char] -> [Char]
clickable icon ws = addActions [ (show i, 1), ("q", 2), ("Left", 4), ("Right", 5) ] icon
                    where i = fromJust $ M.lookup ws myWorkspaceIndices
getColorScheme :: Bool -> (String, String, String)
getColorScheme isDayTime =
 if isDayTime
    then ("#FFFFFF", "#D3D3D3", "#FFA500") -- Light mode: grey2, grey4, orange
    else ("#000000", "#808080", "#FF4500") -- Dark mode: grey2, grey4, orange
myStatusBarSpawner :: ScreenId -> IO StatusBarConfig
myStatusBarSpawner (S s) = do
    colorScheme <- getColorScheme <$> isDayTime
    let statusBarConfig = statusBarPropTo ("_XMONAD_LOG_" ++ show s)
                                          ("xmobar -x " ++ show s ++ " ~/.xmonad/xmobar" ++ show s ++ ".hs")
                                          (return $ myXmobarPP (S s) colorScheme)
    return statusBarConfig

myXmobarPP :: ScreenId -> (String, String, String) -> PP
myXmobarPP s (grey2, grey4, orange) = filterOutWsPP [scratchpadWorkspaceTag] . marshallPP s $ def
 { ppSep = ""
 , ppWsSep = ""
 , ppCurrent = xmobarColor orange ""
 , ppVisible = xmobarColor orange ""
 , ppVisibleNoWindows = Just (xmobarColor grey4 "")
 , ppHidden = xmobarColor grey2 ""
 , ppHiddenNoWindows = xmobarColor grey2 ""
 , ppUrgent = xmobarColor orange ""
 , ppOrder = \(ws : _ : _ : extras) -> ws : extras
 , ppExtras = [ wrapL (actionPrefix ++ "n" ++ actionButton ++ "1>") actionSuffix
                $ wrapL (actionPrefix ++ "q" ++ actionButton ++ "2>") actionSuffix
                $ wrapL (actionPrefix ++ "Left" ++ actionButton ++ "4>") actionSuffix
                $ wrapL (actionPrefix ++ "Right" ++ actionButton ++ "5>") actionSuffix
                $ wrapL "    " "    " $ layoutColorIsActive s (logLayoutOnScreen s)
                , wrapL (actionPrefix ++ "q" ++ actionButton ++ "2>") actionSuffix
                $ titleColorIsActive s (shortenL 95 $ logTitleOnScreen s)
                ]
 }
  where
    titleColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c then xmobarColorL grey2 "" l else xmobarColorL grey3 "" l
    layoutColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c then wrapL "<icon=/home/erel/.xmonad/icons" "_selected.xpm/>" l else wrapL "<icon=/home/erel/.xmonad/icons/" ".xpm/>" l

------------------------------------------------------------------------

main :: IO ()
main = xmonad
       . ewmh
       . ewmhFullscreen
       . dynamicSBs myStatusBarSpawner
       . docks
       $ def
        { focusFollowsMouse  = True
        , clickJustFocuses   = False
        , borderWidth        = 3
        , modMask            = mod4Mask
        , normalBorderColor  = grey2
        , focusedBorderColor = orange
        , terminal           = myTerminal
        , keys               = myKeys
        , workspaces         = withScreens 2 myWorkspaces
        , mouseBindings      = myMouseBindings
        , layoutHook         = myLayoutHook
        , manageHook         = myManageHook
        , startupHook        = myStartupHook

        , rootMask = rootMask def .|. pointerMotionMask
        , logHook            = dynamicLog
                            >> refocusLastLogHook
                            >> nsHideOnFocusLoss myScratchPads
        , handleEventHook    = myHandleEventHook
        } `additionalKeysP` myAditionalKeys
