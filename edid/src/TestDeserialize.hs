{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module TestDeserialize where

import           Data.Bits                  (Bits, complement, shiftL, shiftR,
                                             (.&.))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Serialize.Get
import           Data.Word                  (Word8, Word16, Word64)

-- | An EDID header starts with a static 8 byte header.
--
-- Failing probably isn't the behavior we want here.
header :: Get Word64
header = do
  fixedHeader <- getWord64host
  if 0x00ffffffffffff00 == fixedHeader
     then return fixedHeader
     else fail "This doesn't look like an edid"

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

-- | Parse a manufacturer id from the given binary input.
parseManufacturerId :: Get ByteString
parseManufacturerId = manufacturerId <$> getWord16be

-- | Parse a manufacturer's code from the input.
parseManufacturerCode :: Get Integer
parseManufacturerCode = fromIntegral <$> getWord16le

-- | Parse the serial id for this device.
parseSerialId :: Get Integer
parseSerialId = fromIntegral <$> getWord32le
  -- toStrict . toLazyByteString . word32LE <$> getWord32le


data EdidVersion = V1_0 | V1_1 | V1_2 | V1_3 | V1_4 | V2_0
  deriving (Show, Eq, Ord)

edidVersion :: Word8 -> Word8 -> Maybe EdidVersion
edidVersion 0x1 0x0 = Just V1_0
edidVersion 0x1 0x1 = Just V1_1
edidVersion 0x1 0x2 = Just V1_2
edidVersion 0x1 0x3 = Just V1_3
edidVersion 0x1 0x4 = Just V1_4
edidVersion 0x2 0x0 = Just V2_0
edidVersion _ _ = Nothing

-- | Attempt to parse an EDID binary file
--
-- https://en.wikipedia.org/wiki/Extended_Display_Identification_Data#Structure,_version_1.4
-- https://www.extron.com/article/uedid
parseEdid :: Get (String, ByteString, Integer, Integer, Integer, Integer, Maybe EdidVersion)
parseEdid = do
    _ <- header -- TODO ensure that this is correct header from spec.
    mId <- parseManufacturerId
    mCode <- parseManufacturerCode
    serial <- parseSerialId
    weekOfMan <- fromIntegral <$> getWord8
    yearOfMan <- (+ 1990) . fromIntegral <$> getWord8
    versMaj <- getWord8
    versMin <- getWord8
    rem <- ("Remaining: " ++) . show <$> remaining
    return (rem, mId, mCode, serial, weekOfMan, yearOfMan, edidVersion versMaj versMin)

-- | Test the thing in a REPL
testies :: FilePath -> IO ()
testies fp = do
    file <- BS.readFile fp
    print $ runGet parseEdid file
