-- TODO module header && fn docstrings
module ToggleMon.IO where

import           Control.Exception      (SomeException (..), try)
import           Control.Lens           ((^.))
import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified System.Directory       as Dir
import           ToggleMon.Monad

listDirectory
    :: (MonadReader env m, MonadIO m, HasListDirFn env ListDirectoryAction)
    => Text
    -> m (Maybe [Text])
listDirectory path = do
    env               <- ask
    directoryContents <- liftIO . try $ (env ^. listDirFn) (T.unpack path)
    return
      $ either (\(SomeException _) -> Nothing) Just
      $ (fmap . fmap) T.pack directoryContents

filterDirectory
    :: (MonadReader env m, MonadIO m, HasDisplayBasePath env Text)
    => [Text]
    -> m [Text]
filterDirectory dirs = do
    env <- ask
    let qualifiedPath p = T.unpack (env ^. displayBasePath) <> T.unpack p
    -- TODO the fn from Dir needs to be lifted into the Env reader so it can
    --      be mocked.
    liftIO $ filterM (Dir.doesDirectoryExist . qualifiedPath) dirs

readFile
    :: (MonadReader env m, MonadIO m, HasReadFileFn env ReadFileAction)
    => Text
    -> m Text
readFile path = do
    env      <- ask
    contents <- liftIO $ (env ^. readFileFn) (T.unpack path)
    return . T.strip $ contents

exec
    :: (MonadReader env m, MonadIO m, HasExecFn env ExecuteAction)
    => Text
    -> m Text
exec command = do
    env <- ask
    -- TODO this is an incomplete match :(
    let (cmd : args) = T.splitOn " " command
    results <- liftIO $ (env ^. execFn) (T.unpack cmd) (T.unpack <$> args) []
    return . T.pack $ results
