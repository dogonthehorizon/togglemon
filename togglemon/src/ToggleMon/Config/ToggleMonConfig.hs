{-# LANGUAGE NamedFieldPuns #-}

module ToggleMon.Config.ToggleMonConfig where

import           Control.Monad.Trans            (MonadIO, liftIO)
import           Data.List                      (find)
import           Data.Text                      (Text)
import           Data.Yaml                      (FromJSON, ToJSON)
import qualified Data.Yaml                      as Yaml
import           GHC.Generics                   (Generic)
import qualified System.Environment.XDG.BaseDir as XDG
import qualified ToggleMon.Config.Util          as Util
import           ToggleMon.Display              (Display (..))

data DisplaySettings = DisplaySettings {
    position :: Text,
    scale    :: Text
  } deriving (Show, Generic)

instance ToJSON DisplaySettings
instance FromJSON DisplaySettings

data DisplayConfig  = DisplayConfig {
    name     :: Text,
    settings :: DisplaySettings
  } deriving (Show, Generic)

instance ToJSON DisplayConfig
instance FromJSON DisplayConfig

data ToggleMonConfig = ToggleMonConfig {
    displayConfig :: [DisplayConfig]
  } deriving (Show, Generic)

instance ToJSON ToggleMonConfig
instance FromJSON ToggleMonConfig

getPath :: MonadIO m => m FilePath
getPath = liftIO $
  Util.getPath XDG.getUserConfigDir "togglemon.yaml"

-- TODO unwrap fully qualified name
readConfig :: MonadIO m => m (Either String ToggleMonConfig)
readConfig = getPath >>= Util.readConfig

findInConfig :: ToggleMonConfig -> Text -> Maybe DisplaySettings
findInConfig cfg displayToFind = settings <$> find
  (\DisplayConfig { name } -> name == displayToFind)
  (displayConfig cfg)
