import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

main :: IO ()
main = xmonad =<< xmobar myConfig

myConfig = ewmh def {
                      terminal = myTerminal
                    , modMask = myModMask
                    , borderWidth = myBorderWidth
                    , focusFollowsMouse = myFocusFollowsMouse
                    , layoutHook = myLayoutHook
                    }
                    `additionalKeysP` myKeys

myKeys = [
           ("M-p", spawn "rofi -show run")
         , ("<XF86MonBrightnessDown>", spawn "light -U 2")
         , ("<XF86MonBrightnessUp>", spawn "light -A 2")
         , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
         , ("<XF86AudioMute>", spawn "amixer set Master toggle")
         , ("<XF86AudioMicMute>", spawn "amixer sset Capture toggle")
         , ("<XF86Display>", spawn "xrandr --auto")
         , ("M-<Page_Up>", spawn "transset-df --actual --inc 0.05")
         , ("M-<Page_Down>", spawn "transset-df --actual --dec 0.05")
         ]
myLayoutHook = Tall 1 (3/100) (1/2) ||| Full
myTerminal = "alacritty"
myModMask = mod4Mask
myBorderWidth = 0
myFocusFollowsMouse = True
