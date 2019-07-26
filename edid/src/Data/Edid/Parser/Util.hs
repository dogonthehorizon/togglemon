{-|
Module : Data.Edid.Parser.Util
Description : Utilities for parsing edid values.
Copyright : (c) Fernando Freire, 2019
License : MIT
Maintainer : Fernando Freire
Stability : experimental

Provides utilities for parsing edid values.
-}
module Data.Edid.Parser.Util (
  bitSubRange,
  intToAscii
) where

import Data.Bits (Bits, complement, shiftL, shiftR, (.&.))

-- | Get `n` number of bits from initial position `p` in the given byte-word `w`.
bitSubRange
    :: (Bits a, Num a)
    => a   -- ^ A value that can be subdivided into bits.
    -> Int -- ^ Initial position p from which to start pulling bits.
    -> Int -- ^ The number of bits from initial position p to pull.
    -> a   -- ^ The sub-divided value, same as the input type.
bitSubRange w p n =
    w `shiftR` (p + 1 - n) .&. complement (complement 0 `shiftL` n)


-- | Return the acii character at the provided index.
intToAscii
    :: Int           -- ^ The index of the ascii character to pull.
    -> Maybe Char -- ^ The ascii character if in bounds, 'Nothing' otherwise.
intToAscii i = fmap snd . safeHead . filter ((== i) . fst) $ indexedChars
  where
    indexedChars = zip [1 ..] ['A' .. 'Z']
    safeHead []      = Nothing
    safeHead (x : _) = Just x
