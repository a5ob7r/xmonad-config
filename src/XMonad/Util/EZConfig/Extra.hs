{-# LANGUAGE NamedFieldPuns #-}

module XMonad.Util.EZConfig.Extra (additionalKeys') where

import XMonad
import XMonad.Util.EZConfig (additionalKeys)

-- | A wrapper of 'additionalKeys' to get the current mod-mask key from
-- 'XConfig'.
additionalKeys' :: XConfig a -> (KeyMask -> [((KeyMask, KeySym), X ())]) -> XConfig a
additionalKeys' c@XConfig {modMask} f = c `additionalKeys` f modMask
