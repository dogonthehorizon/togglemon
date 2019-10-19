{-|
Module : Data.Edid.Types
Description : Types representing parts of an EDID blob.
Copyright : (c) Fernando Freire, 2019
License : MIT
Maintainer : Fernando Freire
Stability : experimental

Types that represent various bits in an EDID blob.

Depending on how much information we feel like parsing we could extend this type
quite a bit to also provide available resolutions and such. If that becomes a
viable option then that extends the value of the downstream `togglemon` project.
-}
module Data.Edid.Types where

import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as TE
import           Data.Word          (Word8)
import           GHC.Generics       (Generic)

-- | Version of the EDID spec.
data EdidVersion = V1_0 | V1_1 | V1_2 | V1_3 | V1_4 | V2_0
  deriving (Generic, Show, Eq)

-- | Constructs an EDID version.
mkEdidVersion :: Word8 -> Word8 -> Maybe EdidVersion
mkEdidVersion 0x1 0x0 = Just V1_0
mkEdidVersion 0x1 0x1 = Just V1_1
mkEdidVersion 0x1 0x2 = Just V1_2
mkEdidVersion 0x1 0x3 = Just V1_3
mkEdidVersion 0x1 0x4 = Just V1_4
mkEdidVersion 0x2 0x0 = Just V2_0
mkEdidVersion _   _   = Nothing

-- | Manufacturer information provided by an EDID blob.
data Manufacturer = Manufacturer Text Integer deriving (Generic, Show, Eq)

-- | Constructs a Manufacturer value.
mkManufacturuer :: ByteString -> Integer -> Manufacturer
mkManufacturuer b = Manufacturer (TE.decodeUtf8 b)

-- | Constructs a year of manufacture value.
mkYearOfManufacture :: Word8 -> Integer
mkYearOfManufacture = (+ 1990) . fromIntegral

-- | Representation of a display EDID.
data Edid = Edid {
  manufacturer      :: Manufacturer,
  serialNumber      :: Integer, -- TODO for now
  weekOfManufacture :: Integer, -- TODO for now
  yearOfManufacture :: Integer,
  version           :: EdidVersion
} deriving (Generic, Show, Eq)

-- | An empty EDID object.
empty :: Edid
empty = Edid (Manufacturer "default" 0) 0 0 0 V1_4
