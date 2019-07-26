{-|
Module : Data.Edid.Types
Description : Types representing parts of an EDID blob.
Copyright : (c) Fernando Freire, 2019
License : MIT
Maintainer : Fernando Freire
Stability : experimental

Types that represent various bits in an EDID blob.
-}
module Data.Edid.Types where

import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as TE
import           Data.Word          (Word8)

-- | Version of the EDID spec.
data EdidVersion = V1_0 | V1_1 | V1_2 | V1_3 | V1_4 | V2_0
  deriving (Show, Eq, Ord)

-- | Constructs an EDID version.
edidVersion :: Word8 -> Word8 -> Maybe EdidVersion
edidVersion 0x1 0x0 = Just V1_0
edidVersion 0x1 0x1 = Just V1_1
edidVersion 0x1 0x2 = Just V1_2
edidVersion 0x1 0x3 = Just V1_3
edidVersion 0x1 0x4 = Just V1_4
edidVersion 0x2 0x0 = Just V2_0
edidVersion _   _   = Nothing

-- | Manufacturer information provided by an EDID blob.
data Manufacturer = Manufacturer Text Integer deriving (Show)

-- | Constructs a Manufacturer value.
manufacturer :: ByteString -> Integer -> Manufacturer
manufacturer b = Manufacturer (TE.decodeUtf8 b)
