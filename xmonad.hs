import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStatusBarKey myConfig

myConfig :: XConfig (Choose Tall Full)
myConfig = ewmh def { terminal = "alacritty"
                    , modMask = mod4Mask
                    , borderWidth = 0
                    , focusFollowsMouse = True
                    , layoutHook = myLayoutHook
                    , startupHook = myStartupHook
                    } `additionalKeysP` myKeys

myKeys :: [(String, X ())]
myKeys = [ ("M-p", spawn "rofi -show run")
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
         ]

myStartupHook :: X ()
myStartupHook = do
  spawn "xsetroot -cursor_name left_ptr"
  spawn "[ -f ~/.wallpaper ] && feh --bg-scale ~/.wallpaper"

myLayoutHook :: Choose Tall Full a
myLayoutHook = Tall 1 (3/100) (1/2) ||| Full

myPP :: PP
myPP = xmobarPP { ppSep = " | "
                , ppCurrent = myXmobarColor myYellow . wrap "[" "]"
                , ppTitle = myXmobarColor myGreen . shorten 30
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
