module XMonad.Prompt.Shell.Extra (getTerminal) where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

getTerminal :: IO String
getTerminal = fromMaybe "xterm" <$> (runMaybeT . asum $ MaybeT . lookupEnv <$> ["XMONAD_TERMINAL", "TERMINAL"])
