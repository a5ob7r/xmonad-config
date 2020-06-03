import Data.Char
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStatusBarKey myConfig

myConfig :: XConfig (ModifiedLayout Spacing (Choose ResizableTall Full))
myConfig = ewmh def { terminal = "alacritty"
                    , modMask = mod4Mask
                    , borderWidth = 0
                    , focusFollowsMouse = True
                    , layoutHook = myLayoutHook
                    , startupHook = myStartupHook
                    } `additionalKeysP` myKeys

myKeys :: [(String, X ())]
myKeys = [ ("M-r", spawn "rofi -show run")
         , ("M-w", spawn "rofi -show window")
         , ("<XF86MonBrightnessDown>", spawn "light -U 2")
         , ("<XF86MonBrightnessUp>", spawn "light -A 2")
         , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
         , ("<XF86AudioMute>", spawn "amixer set Master toggle")
         , ("<XF86AudioMicMute>", spawn "amixer sset Capture toggle")
         , ("<XF86Display>", spawn "xrandr --auto")
         , ("M-<Page_Up>", spawn "transset-df --actual --inc 0.05")
         , ("M-<Page_Down>", spawn "transset-df --actual --dec 0.05")
         , ("<Print>", spawn "sh -c 'import -window root ~/Desktop/screen_shot_$(date --iso-8601=seconds).png'")
         , ("M-<Print>", spawn "sh -c 'import ~/Desktop/screen_shot_$(date --iso-8601=seconds).png'")
         , ("M-C-<Print>", spawn "sh -c 'import -window $(xprop -root | grep \"_NET_ACTIVE_WINDOW(WINDOW)\" | sed -e \"s/.* # //g\") ~/Desktop/screen_shot_$(date --iso-8601=seconds).png'")
         , ("M-x", spawn "pkill xmobar")
         , ("M-a", sendMessage MirrorShrink)
         , ("M-z", sendMessage MirrorExpand)
         , ("M-<Return>", spawn "terminal")
         , ("M-C-<Return>", spawn "browser")
         ]

myStartupHook :: X ()
myStartupHook = do
  spawn "xsetroot -cursor_name left_ptr"
  spawn "[ -f ~/.wallpaper ] && feh --bg-scale ~/.wallpaper"

myLayoutHook :: ModifiedLayout Spacing (Choose ResizableTall Full) a
myLayoutHook = mySpacing $ ResizableTall 1 (3/100) (1/2) [] ||| Full

mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacingRaw True myBorder True myBorder True 

myBorder :: Border
myBorder = Border { top = 5
                  , bottom = 5
                  , right = 5
                  , left = 5
                  }

myPP :: PP
myPP = xmobarPP { ppSep = " | "
                , ppCurrent = myXmobarColor myYellow . wrap "[" "]"
                , ppTitle = myXmobarColor myGreen . shortenFW 50
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
myXmobarColor fgColor = xmobarColor fgColor bgColor

toggleStatusBarKey :: XConfig l -> (KeyMask, KeySym)
toggleStatusBarKey XConfig { XMonad.modMask = modMask } = ( modMask, xK_b )

-- | 'shortenFW', is custom version of shorten which treats full width
-- characters' width as double of ascii characters.
shortenFW :: Int -> String -> String
shortenFW n xs = let weights = map (\x -> 1 + fromEnum (not . isAscii $ x)) xs
                     n' = length . takeWhile (<= n) . scanl1 (+) $ weights
                     suffix = if length xs > n' then "..." else ""
                  in take n' xs ++ suffix
