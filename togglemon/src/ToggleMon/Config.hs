module ToggleMon.Config where

import           Data.Edid.Types                (Edid, EdidVersion,
                                                 Manufacturer)
import qualified Data.Edid.Types                as Edid
import           Data.Text                      (Text)
import           Data.Yaml                      (FromJSON, ToJSON, encodeFile)
import           GHC.Generics                   (Generic)
import           System.Environment.XDG.BaseDir (getUserDataDir)
import           System.FilePath                ((</>))

type Name = Text

instance ToJSON Edid
instance FromJSON Edid
instance ToJSON Manufacturer
instance FromJSON Manufacturer
instance ToJSON EdidVersion
instance FromJSON EdidVersion

data HardwareDisplay = HardwareDisplay {
    name :: Text,
    edid :: Edid
  } deriving (Show, Generic)

defaultDisplay :: HardwareDisplay
defaultDisplay = HardwareDisplay "default" Edid.empty

instance ToJSON HardwareDisplay
instance FromJSON HardwareDisplay

data InternalConfig = InternalConfig {
    knownDisplays :: [HardwareDisplay]
  } deriving (Show, Generic)

instance ToJSON InternalConfig
instance FromJSON InternalConfig

data ToggleMonConfig = ToggleMonConfig {
    displayConfig :: [HardwareDisplay]
  }

writeConfig :: InternalConfig -> IO ()
writeConfig config = do
    dataDir <- getUserDataDir "togglemon"
    -- TODO support when xdg-data-dir doesn't exist
    encodeFile (dataDir </> "known-displays.yaml") config
