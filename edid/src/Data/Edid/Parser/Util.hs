module Data.Edid.Parser.Util (
  bitSubRange,
  intToUpperChar
) where

import Data.Bits (Bits, complement, shiftL, shiftR, (.&.))

-- | Get n number of bits from initial position p in the given byte-word w.
bitSubRange :: (Bits a, Num a) => a -> Int -> Int -> a
bitSubRange w p n =
    w `shiftR` (p + 1 - n) .&. complement (complement 0 `shiftL` n)


-- | Return the character at the provided index. Returns 'Nothing' if out of bounds.
intToUpperChar :: Int -> Maybe Char
intToUpperChar i = fmap snd . safeHead . filter ((== i) . fst) $ indexedChars
  where
    indexedChars = zip [1 ..] ['A' .. 'Z']
    safeHead []      = Nothing
    safeHead (x : _) = Just x
