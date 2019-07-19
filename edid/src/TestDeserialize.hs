module TestDeserialize where

import           Data.Bits                  (Bits, complement, shiftL, shiftR,
                                             (.&.))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Serialize.Get
import           Data.Word                  (Word16)

-- | An EDID header starts with a static 8 byte header.
header :: Get ByteString
header = getBytes 8

-- | Get n number of bits from initial position p in the given byte-word w.
bitSubRange :: (Bits a, Num a) => a -> Int -> Int -> a
bitSubRange w p n =
    w `shiftR` (p + 1 - n) .&. complement (complement 0 `shiftL` n)

-- | A manufacturer id is comprised of three letters, each defined in 5 bits.
--
-- A list of PNP manufacturer id can be found here:
-- https://uefi.org/pnp_id_list
--
-- TODO use of !! isn't total, consider changing to reduce chance of surprise.
manufacturerId :: Word16 -> ByteString
manufacturerId w =
    toStrict
        . BLC.pack
        . fmap charIdx
        $ [getBitsAt 14, getBitsAt 9, getBitsAt 4]
  where
    getBitsAt i = bitSubRange w i 5
    charIdx w = ['A' .. 'Z'] !! fromIntegral (w - 1)

-- | Parse a manufacturerId from the given binary input.
parseManufacturerId :: Get ByteString
parseManufacturerId = manufacturerId <$> getWord16be

-- | Attempt to parse an EDID binary file
--
-- https://en.wikipedia.org/wiki/Extended_Display_Identification_Data#Structure,_version_1.4
--
-- TODO next: bytes 10-11 / manufacturer code
parseEdid :: Get ByteString
parseEdid = do
    _ <- header -- TODO ensure that this is correct header from spec.
    parseManufacturerId

-- | Test the thing in a REPL
testies :: FilePath -> IO ()
testies fp = do
    file <- BS.readFile fp
    print $ runGet parseEdid file
