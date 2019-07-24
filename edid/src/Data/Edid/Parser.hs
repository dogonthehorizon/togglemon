{-# LANGUAGE NamedFieldPuns #-}

module Data.Edid.Parser where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Edid.Parser.Util      as Util
import           Data.Edid.Types
import           Data.Serialize.Get
import           Data.Word                  (Word16, Word64)

-- | An EDID header starts with a static 8 byte sequence.
--
-- Failing probably isn't the behavior we want here.
fixedPreamble :: Get Word64
fixedPreamble = do
    fixedHeader <- getWord64host
    if 0x00ffffffffffff00 == fixedHeader
        then return fixedHeader
        else fail "This doesn't look like an edid"

-- | A manufacturer id is comprised of three letters, each defined in 5 bits.
--
-- A list of PNP manufacturer ids can be found here:
-- https://uefi.org/pnp_id_list
manufacturerId :: Word16 -> Maybe ByteString
manufacturerId w =
    fmap (toStrict . BLC.pack)
        . traverse (Util.intToUpperChar . fromIntegral)
        $ [getBitsAt 14, getBitsAt 9, getBitsAt 4]
    where getBitsAt i = Util.bitSubRange w i 5

-- | Parse a manufacturer id from the given binary input.
parseManufacturerId :: Get ByteString
parseManufacturerId = do
    word <- getWord16be
    case manufacturerId word of
        Nothing -> fail "Could not parse word as manufacturer id"
        Just a  -> return a

-- | Parse a manufacturer's code from the input.
--
-- TODO validate that this is correctly parsing
parseManufacturerCode :: Get Integer
parseManufacturerCode = fromIntegral <$> getWord16le

-- | Parse the serial id for this device.
--
-- TODO validate that this is correctly parsing
parseSerialId :: Get Integer
parseSerialId = fromIntegral <$> getWord32le
  -- toStrict . toLazyByteString . word32LE <$> getWord32le

-- | Attempt to parse an EDID binary file
--
-- https://en.wikipedia.org/wiki/Extended_Display_Identification_Data#Structure,_version_1.4
-- https://www.extron.com/article/uedid
parseEdid
    :: Get
           ( String
           , ByteString
           , Integer
           , Integer
           , Integer
           , Integer
           , Maybe EdidVersion
           )
parseEdid = do
    _         <- fixedPreamble
    mId       <- parseManufacturerId
    mCode     <- parseManufacturerCode
    serial    <- parseSerialId
    weekOfMan <- fromIntegral <$> getWord8
    yearOfMan <- (+ 1990) . fromIntegral <$> getWord8
    versMaj   <- getWord8
    versMin   <- getWord8
    rem       <- ("Remaining: " ++) . show <$> remaining
    return
        ( rem
        , mId
        , mCode
        , serial
        , weekOfMan
        , yearOfMan
        , edidVersion versMaj versMin
        )

-- | Test the thing in a REPL
testies :: FilePath -> IO ()
testies fp = do
    file <- BS.readFile fp
    print $ runGet parseEdid file
