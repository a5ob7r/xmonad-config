module Main where

import Data.Char (chr, isAscii, toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStatusBarKey myConfig

myConfig =
  ewmh
    def
      { borderWidth = 1,
        focusFollowsMouse = True,
        layoutHook = myLayoutHook,
        manageHook = myManageHook,
        modMask = mod4Mask,
        startupHook = myStartupHook,
        terminal = "alacritty"
      }
    `additionalKeysP` myKeys

myKeys :: [(String, X ())]
myKeys =
  [ ("M-r", spawn "rofi -show run"),
    ("M-w", spawn "rofi -show window"),
    ("<XF86MonBrightnessDown>", spawn "light -U 2"),
    ("<XF86MonBrightnessUp>", spawn "light -A 2"),
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+"),
    ("<XF86AudioMute>", spawn "amixer set Master toggle"),
    ("<XF86AudioMicMute>", spawn "amixer sset Capture toggle"),
    ("<XF86Display>", spawn "xrandr --auto"),
    ("M-<Page_Up>", spawn "transset-df --actual --inc 0.05"),
    ("M-<Page_Down>", spawn "transset-df --actual --dec 0.05"),
    ("<Print>", spawn "sh -c 'import -window root ~/Desktop/screen_shot_$(date --iso-8601=seconds).png'"),
    ("M-<Print>", spawn "sh -c 'import ~/Desktop/screen_shot_$(date --iso-8601=seconds).png'"),
    ("M-C-<Print>", spawn "sh -c 'import -window $(xprop -root | grep \"_NET_ACTIVE_WINDOW(WINDOW)\" | sed -e \"s/.* # //g\") ~/Desktop/screen_shot_$(date --iso-8601=seconds).png'"),
    ("M-x", spawn "pkill xmobar"),
    ("M-a", sendMessage MirrorShrink),
    ("M-z", sendMessage MirrorExpand),
    ("M-<Return>", raiseTerminal),
    ("M-C-<Return>", raiseBrowser)
  ]

myStartupHook :: X ()
myStartupHook = do
  spawn "xsetroot -cursor_name left_ptr"
  spawn "[ -f ~/.wallpaper ] && feh --bg-scale ~/.wallpaper"

myLayoutHook = full ||| tall
  where
    full = noBorders Full
    tall = mySpacing $ ResizableTall 1 (3 / 100) (1 / 2) []

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

bgColor :: String
bgColor = "#16242c"

myGreen :: String
myGreen = "#99c793"

myYellow :: String
myYellow = "#fac862"

myXmobarColor :: String -> String -> String
myXmobarColor = flip xmobarColor bgColor

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
