module XMonad.Hooks.StatusBar.PP.Extra (shortenFW) where

import Data.Char (isAscii)
import Data.List (scanl')

-- | Custom version of 'XMonad.Hooks.StatusBar.PP.shorten'. This treats full
-- width characters' width as double of ascii characters.
--
-- > shortenFW 3 "foo"
-- "foo"
--
-- > shortenFW 3 "foobar"
-- "..."
--
-- > shortenFW 3 "あいう"
-- "..."
--
-- > shortenFW 5 "あいう"
-- "\12354..."
--
-- > shortenFW 6 "あいう"
-- "\12354\12356\12358"
shortenFW :: Int -> String -> String
shortenFW n s =
  if length s == l
    then s
    else take l' s <> "..."
  where
    weights = (\c -> 1 + fromEnum (not . isAscii $ c)) <$> s
    l = length . takeWhile (<= n) . scanl1 (+) $ weights
    l' = length . tail . takeWhile (<= n) . scanl' (+) 3 $ weights
