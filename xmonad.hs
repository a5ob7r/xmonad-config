module Main where

import Data.Char (chr, isAscii, toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Graphics.X11.ExtraTypes.XF86
import System.Environment (lookupEnv)
import System.FilePath.Posix ((</>))
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.EasyMotion (selectWindow)
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
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
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStatusBarKey myConfig

myConfig =
  ewmh
    def
      { focusedBorderColor = myRed,
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
    ((myModMask, xK_f), selectWindow def >>= (`whenJust` windows . W.focusWindow))
  ]

myStartupHook :: X ()
myStartupHook = do
  spawn "xsetroot -cursor_name left_ptr"
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

myPP :: PP
myPP =
  xmobarPP
    { ppSep = " <fc=#65727d>|</fc> ",
      ppCurrent = myXmobarColor myYellow . wrap "[" "]",
      ppTitle = myXmobarColor myGreen . shortenFW 60
    }

myBar :: String
myBar = "xmobar ~/.xmonad/.xmobarrc"

myBgColor :: String
myBgColor = "#16242c"

myRed :: String
myRed = "#ec5e66"

myGreen :: String
myGreen = "#99c793"

myYellow :: String
myYellow = "#fac862"

myXmobarColor :: String -> String -> String
myXmobarColor = flip xmobarColor myBgColor

toggleStatusBarKey :: XConfig l -> (KeyMask, KeySym)
toggleStatusBarKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- | 'shortenFW', is custom version of shorten which treats full width
-- characters' width as double of ascii characters.
shortenFW :: Int -> String -> String
shortenFW n xs =
  let weights = map (\x -> 1 + fromEnum (not . isAscii $ x)) xs
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
    lowerClassName = fmap (map toLower) appName

getTerminal :: IO String
getTerminal = fromMaybe myTerminal <$> lookupEnv "TERMINAL"

-- Change font config if doesn't show prompt with 'def'.
myXPConfig :: XPConfig
myXPConfig =
  def
    { font = "xft:monospace:size=30:antialias=ture",
      position = Top,
      height = 70,
      searchPredicate = fuzzyMatch,
      sorter = fuzzySort
    }

-- Fixed version of execScriptHook in XMonad.Hooks.Script to put hook scripts
-- to `hooks` directory.
execScriptHook :: String -> X ()
execScriptHook hook = do
  xmonadDir <- asks (cfgDir . directories)
  spawn $ xmonadDir </> "hooks" </> hook
