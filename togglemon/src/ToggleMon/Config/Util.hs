module ToggleMon.Config.Util where

import           Control.Arrow       (left)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Yaml           (FromJSON, ToJSON)
import qualified Data.Yaml           as Yaml
import           System.FilePath     ((</>))

xdgBaseDir :: String
xdgBaseDir = "togglemon"

-- TODO handle when directory or file don't exist
-- TODO handle searching for yml and yaml files
getPath :: MonadIO m => (String -> m FilePath) -> String -> m FilePath
getPath xdgDirGetter fileName = do
  dir <- xdgDirGetter xdgBaseDir
  return $ dir </> fileName

readConfig :: (MonadIO m, FromJSON a) => FilePath -> m (Either String a)
readConfig p = do
  content <- liftIO $ Yaml.decodeFileEither p
  return $ left show content

writeConfig :: (MonadIO m, ToJSON a) => a -> FilePath -> m ()
writeConfig c p = do
  _ <- liftIO $ Yaml.encodeFile p c
  return ()
