module Main (main) where

import Data.Char (isAscii, toLower)
import Data.Map.Strict (member)
import Data.Maybe (fromMaybe)
import Graphics.X11.ExtraTypes.XF86
import System.Environment (lookupEnv)
import System.FilePath.Posix ((</>))
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.EasyMotion (selectWindow)
import XMonad.Actions.WindowGo
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.IfMax
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad $ withEasySB mySB defToggleStrutsKey myConfig

myConfig =
  ewmh
    def
      { focusedBorderColor = red colorscheme,
        layoutHook = myLayoutHook,
        manageHook = myManageHook,
        modMask = myModMask,
        startupHook = myStartupHook,
        terminal = myTerminal
      }
    `additionalKeys` myKeys

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((myModMask, xK_r), shellPrompt myXPConfig),
    ((myModMask .|. controlMask, xK_r), xmonadPrompt myXPConfig),
    ((myModMask, xK_w), windowPrompt myXPConfig Goto allWindows),
    ((noModMask, xF86XK_MonBrightnessUp), spawn "light -A 2"),
    ((noModMask, xF86XK_MonBrightnessDown), spawn "light -U 2"),
    ((noModMask, xF86XK_AudioLowerVolume), spawn "amixer set Master 1%-"),
    ((noModMask, xF86XK_AudioRaiseVolume), spawn "amixer set Master 1%+"),
    ((noModMask, xF86XK_AudioMute), spawn "amixer set Master toggle"),
    ((noModMask, xF86XK_AudioMicMute), spawn "amixer sset Capture toggle"),
    ((noModMask, xF86XK_Display), spawn "xrandr --auto"),
    ((myModMask, xK_u), spawn "picom-trans -c +2"),
    ((myModMask, xK_d), spawn "picom-trans -c -2"),
    ((noModMask, xK_Print), spawn "xcapture root"),
    ((myModMask, xK_Print), spawn "xcapture rect"),
    ((myModMask .|. controlMask, xK_Print), spawn "xcapture active"),
    ((myModMask, xK_a), sendMessage MirrorShrink),
    ((myModMask, xK_z), sendMessage MirrorExpand),
    ((myModMask, xK_Return), raiseTerminal),
    ((myModMask .|. controlMask, xK_Return), raiseBrowser),
    ((myModMask, xK_v), windows copyToAll),
    ((myModMask, xK_x), killAllOtherCopies),
    ((myModMask, xK_f), selectWindow def >>= (`whenJust` windows . W.focusWindow)),
    ((myModMask, xK_j), windows W.focusDown >> whenX isCurrentActiveFloating (windows W.focusDown)),
    ((myModMask, xK_k), windows W.focusUp >> whenX isCurrentActiveFloating (windows W.focusUp))
  ]

myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  execScriptHook "wallpaper"

myLayoutHook = full ||| tall ||| htall
  where
    full = renamed [Replace "Full"] $ noBorders Full
    tall = renamed [Replace "Tall"] . IfMax 1 full . mySpacing $ ResizableTall nMaster delta ratio []
    htall = renamed [Replace "HTall"] . IfMax 1 full . mySpacing $ ResizableTall 0 delta ratio []
    nMaster = 1
    delta = 3 / 100
    ratio = 1 / 2

mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacingRaw True myBorder True myBorder True

myBorder :: Border
myBorder =
  Border
    { top = 5,
      bottom = 5,
      right = 5,
      left = 5
    }

mySB :: StatusBarConfig
mySB = statusBarProp myBar $ pure myPP

myPP :: PP
myPP =
  xmobarPP
    { ppSep = " <fc=" <> brightBlack colorscheme <> ">|</fc> ",
      ppCurrent = myXmobarColor (yellow colorscheme) . wrap "[" "]",
      ppLayout = xmobarBorder "Bottom" (white colorscheme) 4,
      ppTitle = xmobarBorder "Bottom" (green colorscheme) 4 . myXmobarColor (green colorscheme) . shortenFW 60
    }

myBar :: String
myBar = "xmobar"

colorscheme :: OceanicNext
colorscheme = OceanicNext

myXmobarColor :: String -> String -> String
myXmobarColor = flip xmobarColor (background colorscheme)

toggleStatusBarKey :: XConfig l -> (KeyMask, KeySym)
toggleStatusBarKey XConfig {XMonad.modMask = mask} = (mask, xK_b)

-- | 'shortenFW', is custom version of shorten which treats full width
-- characters' width as double of ascii characters.
shortenFW :: Int -> String -> String
shortenFW n xs =
  let weights = (\x -> 1 + fromEnum (not . isAscii $ x)) <$> xs
      n' = length . takeWhile (<= n) . scanl1 (+) $ weights
      suffix = if length xs > n' then "..." else ""
   in take n' xs ++ suffix

-- | Return whether or not a window is picture in picture(PIP) for Firefox.
-- Somehow can't detect using `WM_NAME` (is title, ) that a window is PIP on
-- Firefox. Why?
-- ref: https://wiki.haskell.org/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program
isPIPFF :: Query Bool
isPIPFF = q1 <||> q2
  where
    -- Since Firefox Developer Edition Ver. 90.0*.
    q1 = title =? "Picture-in-Picture"
    q2 = stringProperty "WM_WINDOW_ROLE" =? "PictureInPicture"

-- | Return whether or not a window is picture in picture(PIP) for Chrome.
isPIPChrome :: Query Bool
isPIPChrome = title =? "Picture in picture"

-- | Return whether or not a window is picture in picture(PIP).
isPIP :: Query Bool
isPIP = isPIPFF <||> isPIPChrome

myManageHook :: ManageHook
myManageHook = isPIP --> doFloat

raiseTerminal :: X ()
raiseTerminal = liftIO getTerminal >>= \term -> runOrRaise term (lowerClassName =? term)
  where
    lowerClassName = map toLower <$> appName

getTerminal :: IO String
getTerminal = fromMaybe myTerminal <$> lookupEnv "TERMINAL"

-- Change font config if doesn't show prompt with 'def'.
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

-- Fixed version of execScriptHook in XMonad.Hooks.Script to put hook scripts
-- to `hooks` directory.
execScriptHook :: String -> X ()
execScriptHook script = do
  xmonadDir <- asks (cfgDir . directories)
  spawn $ xmonadDir </> "hooks" </> script

-- Whether or not current active window is floating.
isCurrentActiveFloating :: X Bool
isCurrentActiveFloating = do
  s <- get
  let w = fmap W.focus . W.stack . W.workspace . W.current $ windowset s
  case w of
    Just w' -> pure . member w' . W.floating $ windowset s
    Nothing -> pure False

-- | Terminal ColorScheme
class ColorScheme a where
  background :: a -> String
  foreground :: a -> String

  black :: a -> String
  red :: a -> String
  green :: a -> String
  yellow :: a -> String
  blue :: a -> String
  magenta :: a -> String
  cyan :: a -> String
  white :: a -> String

  brightBlack :: a -> String
  brightBlack = black

  brightRed :: a -> String
  brightRed = red

  brightGreen :: a -> String
  brightGreen = green

  brightYellow :: a -> String
  brightYellow = yellow

  brightBlue :: a -> String
  brightBlue = blue

  brightMagenta :: a -> String
  brightMagenta = magenta

  brightCyan :: a -> String
  brightCyan = cyan

  brightWhite :: a -> String
  brightWhite = white

data OceanicNext = OceanicNext

instance ColorScheme OceanicNext where
  background = const "#16242c"
  foreground = const "#c0c4cd"

  black = const "#16252b"
  red = const "#ec5e66"
  green = const "#99c793"
  yellow = const "#fac862"
  blue = const "#6699cb"
  magenta = const "#c593c4"
  cyan = const "#5fb3b2"
  white = const "#fefefe"

  brightBlack = const "#65727d"
  brightRed = const "#ec5e66"
  brightGreen = const "#99c793"
  brightYellow = const "#fac862"
  brightBlue = const "#6699cb"
  brightMagenta = const "#c593c4"
  brightCyan = const "#5fb3b2"
  brightWhite = const "#fefefe"

data TomorrowNight = TomorrowNight

instance ColorScheme TomorrowNight where
  background = const "#1d1f21"
  foreground = const "#c5c8c6"

  black = const "#1d1f21"
  red = const "#cc6666"
  green = const "#b5bd68"
  yellow = const "#f0c674"
  blue = const "#81a2be"
  magenta = const "#b294bb"
  cyan = const "#8abeb7"
  white = const "#c5c8c6"

  brightBlack = const "#666666"
  brightRed = const "#d54e53"
  brightGreen = const "#b9ca4a"
  brightYellow = const "#e7c547"
  brightBlue = const "#7aa6da"
  brightMagenta = const "#c397d8"
  brightCyan = const "#70c0b1"
  brightWhite = const "#eaeaea"
