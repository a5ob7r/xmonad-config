{-# OPTIONS_GHC -Wno-missing-signatures #-}

module XMonad.A5ob7r (myxmonad) where

import Data.List (find, intersperse)
import Data.Maybe (isJust)
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xrandr (xrrGetMonitors, xrr_moninf_primary, xrr_moninf_width)
import XMonad
import XMonad.Actions.EasyMotion (selectWindow)
import XMonad.Actions.WindowGo (raiseBrowser)
import XMonad.Actions.WindowGo.Extra (raiseTerminal)
import XMonad.Config.A5ob7r.ColorScheme
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageHelpers (isInProperty)
import XMonad.Hooks.Rescreen (RescreenConfig (..), rescreenHook)
import XMonad.Hooks.Script (execScriptHook)
import XMonad.Hooks.StatusBar (StatusBarConfig, defToggleStrutsKey, statusBarProp, withEasySB)
import XMonad.Hooks.StatusBar.PP (PP (..), wrap, xmobarBorder, xmobarColor, xmobarPP)
import XMonad.Hooks.StatusBar.PP.Extra (shortenFW)
import XMonad.Layout.FocusTracking (focusTracking)
import XMonad.Layout.IfMax (IfMax (IfMax))
import XMonad.Layout.Renamed (named)
import XMonad.Layout.ResizableTile (MirrorResize (..), ResizableTall (..))
import XMonad.Layout.VoidBorders (normalBorders, voidBorders)
import XMonad.Operations.Extra (focusDownWithFloatSkipping, focusUpWithFloatSkipping)
import XMonad.Prompt (XPConfig (..), XPPosition (..))
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Shell.Extra (getTerminal)
import XMonad.Prompt.Window (WindowPrompt (..), allWindows, windowPrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)
import XMonad.StackSet qualified as W
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig.Extra (additionalKeys')
import XMonad.Util.StickyWindows (stick, sticky, unstick)

myxmonad :: IO ()
myxmonad = do
  myTerminal <- getTerminal

  xmonad . withEasySB mySB defToggleStrutsKey . sticky . ewmh . rescreenHook myRescreenConfig $
    def
      { focusedBorderColor = red colorscheme,
        layoutHook = myLayoutHook,
        manageHook = myManageHook,
        modMask = mod4Mask,
        startupHook = myStartupHook,
        terminal = myTerminal
      }
      `additionalKeys'` \mask ->
        [ ((mask, xK_r), shellPrompt myXPConfig),
          ((mask .|. controlMask, xK_r), xmonadPrompt myXPConfig),
          ((mask, xK_w), windowPrompt myXPConfig Goto allWindows),
          ((noModMask, xF86XK_MonBrightnessUp), spawn "brightnessctl set 1%+"),
          ((noModMask, xF86XK_MonBrightnessDown), spawn "brightnessctl set 1%-"),
          ((noModMask, xF86XK_AudioLowerVolume), spawn "amixer set Master 1%-"),
          ((noModMask, xF86XK_AudioRaiseVolume), spawn "amixer set Master 1%+"),
          ((noModMask, xF86XK_AudioMute), spawn "amixer set Master toggle"),
          ((noModMask, xF86XK_AudioMicMute), spawn "amixer sset Capture toggle"),
          ((noModMask, xF86XK_Display), spawn "autorandr --change"),
          ((mask, xK_u), spawn "picom-trans -c +2"),
          ((mask, xK_d), spawn "picom-trans -c -2"),
          ((noModMask, xK_Print), spawn "xcapture root"),
          ((mask, xK_Print), spawn "xcapture rect"),
          ((mask .|. controlMask, xK_Print), spawn "xcapture active"),
          ((mask, xK_a), sendMessage MirrorShrink),
          ((mask, xK_z), sendMessage MirrorExpand),
          ((mask, xK_Return), raiseTerminal),
          ((mask .|. controlMask, xK_Return), raiseBrowser),
          ((mask, xK_v), withFocused stick),
          ((mask, xK_x), withFocused unstick),
          ((mask, xK_f), selectWindow def >>= (`whenJust` windows . W.focusWindow)),
          ((mask, xK_j), focusDownWithFloatSkipping),
          ((mask, xK_k), focusUpWithFloatSkipping)
        ]

myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  execScriptHook "wallpaper"

myRescreenConfig :: RescreenConfig
myRescreenConfig = def {afterRescreenHook, randrChangeHook}
  where
    afterRescreenHook = execScriptHook "wallpaper"
    randrChangeHook = execScriptHook "rescreen"

-- | I'm not sure, but inserting spaces around windows looks so cool. However
-- it wastes display areas, so don't use it. Instead, we probably need
-- bordering around the windows to indicate the window focus.
myLayoutHook = full ||| tall
  where
    full = named "Full" . focusTracking $ voidBorders Full

    -- The Tall layout which has no master area means just equal size multiple
    -- rows and just a single column. If you want to use two column layout,
    -- increase the number of master windows using 'IncMasterN' as below.
    --
    -- > sendMessage (IncMasterN 1)
    --
    -- Or, decrease it to restore a single column layout as below.
    --
    -- > sendMessage (IncMasterN (-1))
    --
    -- By default, Xmonad binds these actions to @Mod+,@ and @Mod-.@.
    tall = named "Tall" . IfMax 1 full . normalBorders $ ResizableTall nMaster delta ratio []
    nMaster = 0
    delta = 3 / 100
    ratio = 1 / 2

mySB :: StatusBarConfig
mySB = statusBarProp "xmobar" $ do
  wset <- gets windowset

  -- Get the primary monitor's width(pixels).
  monitorWidthPixels <- maybe (fromIntegral $ currentScreenWidth wset) xrr_moninf_width <$> primaryMonitor

  -- Assume single xmobar instance is only on the primary monitor.
  let availableDisplayWidth = fromIntegral monitorWidthPixels `div` glyphWidthPixels
      availableTitleDisplayWidth = max 5 $ availableDisplayWidth - (lhsDisplayWidth wset + rhsDisplayWidth wset)
   in return $
        xmobarPP
          { ppSep = "  ",
            ppCurrent = xmobarUnderline (yellow colorscheme) 4 . xmobarColor' (yellow colorscheme) . wrap "[" "]",
            ppVisible = xmobarUnderline (yellow colorscheme) 4 . wrap "(" ")",
            ppHidden = xmobarUnderline (yellow colorscheme) 4,
            ppUrgent = xmobarUnderline (yellow colorscheme) 4 . xmobarColor' (red colorscheme),
            ppWsSep = " ",
            ppLayout = xmobarUnderline (yellow colorscheme) 4,
            ppTitle = xmobarUnderline (yellow colorscheme) 4 . xmobarColor' (foreground colorscheme) . shortenFW availableTitleDisplayWidth
          }
  where
    xmobarColor' fg = xmobarColor fg $ background colorscheme
    xmobarUnderline = xmobarBorder "Bottom"

    lhsDisplayWidth wset = 1 + workspaceDisplayWidth wset + 2 + currentLayoutDisplayWidth wset + 2
    workspaceDisplayWidth wset =
      sum . intersperse 1 $
        [1 + length (W.currentTag wset) + 1] -- "[1]"
          <> [1 + length (W.tag . W.workspace $ screen) + 1 | screen <- W.visible wset] -- "(2)"
          <> [length . W.tag $ workspace | workspace <- W.hidden wset, hasWindow workspace] -- "3"
    hasWindow = isJust . W.stack
    currentLayoutDisplayWidth = length . description . W.layout . W.workspace . W.current
    rhsDisplayWidth = const 70

    currentScreenWidth = rect_width . screenRect . W.screenDetail . W.current
    primaryMonitor = do
      dpy <- asks display
      monitors <- io $ xrrGetMonitors dpy (defaultRootWindow dpy) True
      pure $ monitors >>= find xrr_moninf_primary

    -- A monospace glyph width(pixels) on xmobar.
    glyphWidthPixels = ceiling $ fontPoints * fontPointScale * inchesPerPoint * pixelsPerInch
    fontPoints = 30 -- from 'font' option in xmobar configuratioon
    fontPointScale = 1 / 2 -- for Hackgen(Halfwidth:Fullwidth = 1:2)
    inchesPerPoint = 1 / 72
    pixelsPerInch = 96 :: Rational -- DPI

colorscheme :: OceanicNext
colorscheme = OceanicNext

myManageHook :: ManageHook
myManageHook = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE" --> doFloat

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
