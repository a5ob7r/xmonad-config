module XMonad.Actions.WindowGo.Extra (raiseTerminal) where

import Data.List.Extra (lower)
import XMonad
import XMonad.Actions.WindowGo (runOrRaise)

-- | A terminal version of 'raiseBrowser' or 'raiseEditor' in
-- "XMonad.Actions.WindowGo" with extra.
--
-- Like above two function, the terminal appcaliation's name is
-- case-insensitive.
--
-- Unlike above two function, this queries not only 'className', but also
-- 'appName' using the terminal appcaliation's name.
--
-- * 'appName' is the first element in @WM_CLASS(STRING)@.
-- * 'className' is the second element in @WM_CLASS(STRING)@.
-- <https://wiki.haskell.org/Xmonad/Frequently_asked_questions>
raiseTerminal :: X ()
raiseTerminal = do
  term <- asks $ terminal . config
  runOrRaise term $ (lower <$> className) =? term <||> (lower <$> appName) =? term
