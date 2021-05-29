module Main where

import Data.Char (chr, isAscii, toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Graphics.X11.ExtraTypes.XF86
import System.Environment (lookupEnv)
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStatusBarKey myConfig

myConfig =
  ewmh
    def
      { layoutHook = myLayoutHook,
        manageHook = myManageHook,
        modMask = myModMask,
        startupHook = myStartupHook,
        terminal = "alacritty"
      }
    `additionalKeys` myKeys

myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((myModMask, xK_r), shellPrompt myXPConfig),
    ((myModMask .|. controlMask, xK_r), xmonadPrompt myXPConfig),
    ((myModMask, xK_w), windowPrompt myXPConfig Goto allWindows),
    ((noModMask, xF86XK_MonBrightnessUp), spawn "light -A 2"),
    ((noModMask, xF86XK_MonBrightnessDown), spawn "light -U 2"),
    ((noModMask, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-"),
    ((noModMask, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+"),
    ((noModMask, xF86XK_AudioMute), spawn "amixer set Master toggle"),
    ((noModMask, xF86XK_AudioMicMute), spawn "amixer sset Capture toggle"),
    ((noModMask, xF86XK_Display), spawn "xrandr --auto"),
    ((myModMask, xK_KP_Page_Up), spawn "transset-df --actual --inc 0.05"),
    ((myModMask, xK_KP_Page_Down), spawn "transset-df --actual --dec 0.05"),
    ((noModMask, xK_Print), spawn "import -window root ~/Desktop/screen_shot_$(date --iso-8601=seconds).png"),
    ((myModMask, xK_Print), spawn "import ~/Desktop/screen_shot_$(date --iso-8601=seconds).png"),
    ((myModMask .|. controlMask, xK_Print), spawn "xprop -root | grep '_NET_ACTIVE_WINDOW(WINDOW)' | sed -e 's/.* # //g' | xargs -I '{}' import -window '{}' ~/Desktop/screen_shot_$(date --iso-8601=seconds).png"),
    ((myModMask, xK_a), sendMessage MirrorShrink),
    ((myModMask, xK_z), sendMessage MirrorExpand),
    ((myModMask, xK_Return), raiseTerminal),
    ((myModMask .|. controlMask, xK_Return), raiseBrowser),
    ((myModMask, xK_v), windows copyToAll),
    ((myModMask, xK_x), killAllOtherCopies)
  ]

myStartupHook :: X ()
myStartupHook = do
  spawn "xsetroot -cursor_name left_ptr"
  spawn "[ -f ~/.wallpaper ] && feh --bg-scale ~/.wallpaper"

myLayoutHook = full ||| tall
  where
    full = noBorders Full
    tall = mySpacing $ ResizableTall nMaster delta ratio []
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
    { ppSep = wrap " " " " [chr 57535],
      ppCurrent = myXmobarColor myYellow . wrap "[" "]",
      ppTitle = myXmobarColor myGreen . shortenFW 60,
      ppLayout = convertLayoutName
    }

myBar :: String
myBar = "xmobar ~/.xmonad/.xmobarrc"

myBgColor :: String
myBgColor = "#16242c"

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

convertLayoutName :: String -> String
convertLayoutName name
  | "Full" `isInfixOf` name = "Full"
  | "Tall" `isInfixOf` name = "Tall"
  | otherwise = "Unknown"

-- | Return whether or not a window is picture in picture(PIP) for Firefox.
-- Somehow can't detect using `WM_NAME` (is title, ) that a window is PIP on
-- Firefox. Why?
-- ref: https://wiki.haskell.org/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program
isPIPFF :: Query Bool
isPIPFF = stringProperty "WM_WINDOW_ROLE" =? "PictureInPicture"

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
    lowerClassName = fmap (map toLower) className

getTerminal :: IO String
getTerminal = fromMaybe "alacritty" <$> lookupEnv "TERMINAL"

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
