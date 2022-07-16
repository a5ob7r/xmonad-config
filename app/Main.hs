{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Char (isAscii, toLower)
import Data.List (scanl')
import Data.Map.Strict (member)
import Data.Maybe (fromMaybe)
import Graphics.X11.ExtraTypes.XF86
import System.Environment (lookupEnv)
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.EasyMotion (selectWindow)
import XMonad.Actions.WindowGo
import XMonad.Config.A5ob7r.ColorScheme
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Rescreen
import XMonad.Hooks.Script
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.IfMax
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.EZConfig

main :: IO ()
main =
  xmonad . withEasySB mySB defToggleStrutsKey . ewmh . rescreenHook myRescreenConfig $
    def
      { focusedBorderColor = red colorscheme,
        layoutHook = myLayoutHook,
        manageHook = myManageHook,
        modMask = mod4Mask,
        startupHook = myStartupHook,
        terminal = "alacritty"
      }
      `additionalKeys'` \mask ->
        [ ((mask, xK_r), shellPrompt myXPConfig),
          ((mask .|. controlMask, xK_r), xmonadPrompt myXPConfig),
          ((mask, xK_w), windowPrompt myXPConfig Goto allWindows),
          ((noModMask, xF86XK_MonBrightnessUp), spawn "light -A 2"),
          ((noModMask, xF86XK_MonBrightnessDown), spawn "light -U 2"),
          ((noModMask, xF86XK_AudioLowerVolume), spawn "amixer set Master 1%-"),
          ((noModMask, xF86XK_AudioRaiseVolume), spawn "amixer set Master 1%+"),
          ((noModMask, xF86XK_AudioMute), spawn "amixer set Master toggle"),
          ((noModMask, xF86XK_AudioMicMute), spawn "amixer sset Capture toggle"),
          ((noModMask, xF86XK_Display), spawn "xrandr --auto"),
          ((mask, xK_u), spawn "picom-trans -c +2"),
          ((mask, xK_d), spawn "picom-trans -c -2"),
          ((noModMask, xK_Print), spawn "xcapture root"),
          ((mask, xK_Print), spawn "xcapture rect"),
          ((mask .|. controlMask, xK_Print), spawn "xcapture active"),
          ((mask, xK_a), sendMessage MirrorShrink),
          ((mask, xK_z), sendMessage MirrorExpand),
          ((mask, xK_Return), raiseTerminal),
          ((mask .|. controlMask, xK_Return), raiseBrowser),
          ((mask, xK_v), windows copyToAll),
          ((mask, xK_x), killAllOtherCopies),
          ((mask, xK_f), selectWindow def >>= (`whenJust` windows . W.focusWindow)),
          ((mask, xK_j), windows W.focusDown >> whenX isCurrentActiveFloating (windows W.focusDown)),
          ((mask, xK_k), windows W.focusUp >> whenX isCurrentActiveFloating (windows W.focusUp))
        ]

-- | A wrapper of 'additionalKeys' to get the current mod-mask key from
-- 'XConfig'.
additionalKeys' :: XConfig a -> (KeyMask -> [((KeyMask, KeySym), X ())]) -> XConfig a
additionalKeys' c@XConfig {..} f = c `additionalKeys` f modMask

myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  execScriptHook "wallpaper"

myRescreenConfig :: RescreenConfig
myRescreenConfig = RescreenConfig {..}
  where
    afterRescreenHook = execScriptHook "wallpaper"
    randrChangeHook = execScriptHook "rescreen"

-- | I'm not sure, but inserting spaces around windows looks so cool. However
-- it wastes display areas, so don't use it. Instead, we probably need
-- bordering around the windows to indicate the window focus.
myLayoutHook = full ||| tall ||| htall
  where
    full = renamed [Replace "Full"] $ noBorders Full
    tall = renamed [Replace "Tall"] . IfMax 1 full $ ResizableTall nMaster delta ratio []
    htall = renamed [Replace "HTall"] . IfMax 1 full $ ResizableTall 0 delta ratio []
    nMaster = 1
    delta = 3 / 100
    ratio = 1 / 2

mySB :: StatusBarConfig
mySB =
  statusBarProp "xmobar" $ do
    width <- gets $ rect_width . screenRect . W.screenDetail . W.current . windowset

    pure $
      xmobarPP
        { ppSep = wrap " " " " $ xmobarColor' (brightBlack colorscheme) ":",
          ppCurrent = xmobarColor' (yellow colorscheme) . wrap "[" "]",
          ppLayout = xmobarUnderline (white colorscheme) 4,
          ppTitle = xmobarUnderline (green colorscheme) 4 . xmobarColor' (green colorscheme) . shortenFW (max 60 (fromIntegral width - 2000) `div` 30)
        }
  where
    xmobarColor' fg = xmobarColor fg $ background colorscheme
    xmobarUnderline = xmobarBorder "Bottom"

colorscheme :: OceanicNext
colorscheme = OceanicNext

-- | Custom version of 'XMonad.Hooks.StatusBar.PP.shorten'. This treats full
-- width characters' width as double of ascii characters.
--
-- > shortenFW 3 "foo"
-- "foo"
--
-- > shortenFW 3 "foobar"
-- "..."
--
-- > shortenFW 3 "あいう"
-- "..."
--
-- > shortenFW 5 "あいう"
-- "\12354..."
--
-- > shortenFW 6 "あいう"
-- "\12354\12356\12358"
shortenFW :: Int -> String -> String
shortenFW n s =
  if length s == l
    then s
    else take l' s <> "..."
  where
    weights = (\c -> 1 + fromEnum (not . isAscii $ c)) <$> s
    l = length . takeWhile (<= n) . scanl1 (+) $ weights
    l' = length . tail . takeWhile (<= n) . scanl' (+) 3 $ weights

myManageHook :: ManageHook
myManageHook = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE" --> doFloat

-- | A terminal version of 'raiseBrowser' or 'raiseEditor' in
-- "XMonad.Actions.WindowGo" with extra.
--
-- Like above two function, the terminal appcaliation's name is
-- case-insensitive.
--
-- Unlike above two function, this queries not only 'className', but also
-- 'appName' using the terminal appcaliation's name.
--
-- * 'className' is the first element in @WM_CLASS(STRING)@.
-- * 'appName' is the second element in @WM_CLASS(STRING)@.
-- <https://wiki.haskell.org/Xmonad/Frequently_asked_questions>
--
-- The terminal's application name is determined in the following order.
--
-- 1. Looks up @$TERMINAL@.
-- 2. Fallbacks to your 'terminal' in 'XConfig'.
raiseTerminal :: X ()
raiseTerminal = do
  XConf {config = XConfig {..}} <- ask

  term <- liftIO $ fromMaybe terminal <$> lookupEnv "TERMINAL"
  runOrRaise term (className' =? term <||> appName' =? term)
  where
    className' = map toLower <$> className
    appName' = map toLower <$> appName

-- | Change font config if doesn't show prompt with 'def'.
myXPConfig :: XPConfig
myXPConfig =
  def
    { font = "xft:monospace:size=30:antialias=ture",
      bgColor = background OceanicNext,
      fgColor = foreground OceanicNext,
      bgHLight = brightBlack OceanicNext,
      fgHLight = yellow OceanicNext,
      borderColor = foreground OceanicNext,
      position = Top,
      height = 70,
      searchPredicate = fuzzyMatch,
      sorter = fuzzySort
    }

-- | Whether or not current active window is floating.
isCurrentActiveFloating :: X Bool
isCurrentActiveFloating = do
  ws <- gets windowset

  case W.peek ws of
    Just w -> pure . member w $ W.floating ws
    Nothing -> pure False
