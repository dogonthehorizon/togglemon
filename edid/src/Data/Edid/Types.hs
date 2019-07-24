module Data.Edid.Types where

import Data.Word (Word8)

data EdidVersion = V1_0 | V1_1 | V1_2 | V1_3 | V1_4 | V2_0
  deriving (Show, Eq, Ord)

edidVersion :: Word8 -> Word8 -> Maybe EdidVersion
edidVersion 0x1 0x0 = Just V1_0
edidVersion 0x1 0x1 = Just V1_1
edidVersion 0x1 0x2 = Just V1_2
edidVersion 0x1 0x3 = Just V1_3
edidVersion 0x1 0x4 = Just V1_4
edidVersion 0x2 0x0 = Just V2_0
edidVersion _   _   = Nothing

