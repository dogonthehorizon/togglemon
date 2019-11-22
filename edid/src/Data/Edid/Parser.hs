{-|
Module : Data.Edid.Parser
Description : Parser combinators and functions for EDID blobs
Copyright : (c) Fernando Freire, 2019
License : MIT
Maintainer : Fernando Freire
Stability : experimental

This module provides parser combinators and functions for parsing EDID
blobs. There are a few versions of the EDID spec, this module focuses on 1.4
as outlined in this page:

https://en.wikipedia.org/wiki/Extended_Display_Identification_Data#Structure,_version_1.4

Supposedly the spec is forwards compatible, but I don't have enough monitors
to test whether that is the case or not.

TODO provide typical usage instructions once the API is a bit more stable.
-}
module Data.Edid.Parser (
  fixedPreamble,
  manufacturerId,
  parseContent,
  parseFile,
  parseManufacturerCode,
  parseManufacturerId,
  parseSerialId
) where

import           Control.Monad              (liftM2)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Edid.Parser.Util      as Util
import           Data.Edid.Types
import           Data.Serialize.Get
import           Data.Word                  (Word16, Word64)

-- | An EDID header starts with a static 8 byte sequence.
fixedPreamble :: Get Word64
fixedPreamble = do
    fixedHeader <- getWord64host
    if 0x00ffffffffffff00 == fixedHeader
        then return fixedHeader
        else fail
            "Didn't find static EDID header, are we reading the right file?"

-- | A manufacturer id is comprised of three letters, each defined in five bits.
--
-- A list of PNP manufacturer ids can be found here:
-- https://uefi.org/pnp_id_list
manufacturerId :: Word16 -> Maybe ByteString
manufacturerId w =
    fmap (toStrict . BLC.pack)
        . traverse (Util.intToAscii . fromIntegral)
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

parseVersion :: Get EdidVersion
parseVersion = do
    major <- getWord8
    minor <- getWord8

    case mkEdidVersion major minor of
        Just version -> return version
        Nothing ->
            fail
                $  "Unable to parse edid version, got bytes: '"
                <> show major
                <> "' '"
                <> show minor
                <> "'"

-- | Attempt to parse an EDID binary file
--
-- https://en.wikipedia.org/wiki/Extended_Display_Identification_Data#Structure,_version_1.4
-- https://www.extron.com/article/uedid
parseEdid :: Get Edid
parseEdid = do
    _ <- fixedPreamble

    Edid
        <$> liftM2 mkManufacturuer parseManufacturerId parseManufacturerCode
        <*> parseSerialId
        <*> (fromIntegral <$> getWord8)
        <*> (mkYearOfManufacture <$> getWord8)
        <*> parseVersion

-- | Attempt to parse EDID from provided content.
--
-- https://en.wikipedia.org/wiki/Extended_Display_Identification_Data#Structure,_version_1.4
-- https://www.extron.com/article/uedid
parseContent :: ByteString -> Either String Edid
parseContent = runGet parseEdid

-- | Attempt to parse EDID from provided file path.
--
-- https://en.wikipedia.org/wiki/Extended_Display_Identification_Data#Structure,_version_1.4
-- https://www.extron.com/article/uedid
parseFile :: FilePath -> IO (Either String Edid)
parseFile path = do
    file <- BS.readFile path
    return $ parseContent file
