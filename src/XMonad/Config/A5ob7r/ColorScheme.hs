module XMonad.Config.A5ob7r.ColorScheme
  ( ColorScheme (..),
    OceanicNext (..),
    TomorrowNight (..),
  )
where

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
