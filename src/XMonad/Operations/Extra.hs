{-# LANGUAGE LambdaCase #-}

module XMonad.Operations.Extra (focusUpWithFloatSkipping, focusDownWithFloatSkipping) where

import Control.Monad ((>=>))
import Control.Monad.Extra (allM)
import Data.Function (fix)
import XMonad
import XMonad.Hooks.RefocusLast (isFloat)
import qualified XMonad.StackSet as W

focusUpWithFloatSkipping :: X ()
focusUpWithFloatSkipping = focusSkipFloat W.focusUp

focusDownWithFloatSkipping :: X ()
focusDownWithFloatSkipping = focusSkipFloat W.focusDown

-- | A wrapper of 'windows' to skip floating windows when switch the window
-- focus. If all of windows on the workspace is floating, this function runs
-- @f@ just once. Otherwise this function runs @f@ until fucuses a non-floating
-- window.
--
-- FIXME: This function assumes that @f@ is only 'W.focusDown' or 'W.focusUp'.
-- This means that @f@ must rotate the window focus. If @f@ doesn't it, this
-- function probably causes an infinite loop.
focusSkipFloat :: (WindowSet -> WindowSet) -> X ()
focusSkipFloat f =
  withWindowSet $
    allM (runQuery isFloat) . W.index >=> \case
      True -> windows f
      False -> windows f >> fix (\more -> whenX isFocusFloat $ windows f >> more)

-- | Whether or not the focused window on the current workspace is floating.
isFocusFloat :: X Bool
isFocusFloat = withWindowSet $ maybe (return False) (runQuery isFloat) . W.peek
