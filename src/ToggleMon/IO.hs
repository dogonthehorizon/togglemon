-- TODO module header && fn docstrings
module ToggleMon.IO where

import           Control.Lens           ((^.))
import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified System.Directory       as Dir
import           ToggleMon.Monad

listDirectory
    :: (MonadReader env m, MonadIO m, HasListDirFn env ListDirectory)
    => Text
    -> m [Text]
listDirectory path = do
    env <- ask
    (fmap . fmap) T.pack $ liftIO $ (env ^. listDirFn) (T.unpack path)

filterDirectory
    :: (MonadReader env m, MonadIO m, HasDisplayBasePath env Text)
    => [Text]
    -> m [Text]
filterDirectory dirs = do
    env <- ask
    let qualifiedPath p = T.unpack (env ^. displayBasePath) <> T.unpack p
    liftIO $ filterM (Dir.doesDirectoryExist . qualifiedPath) dirs

readFile
    :: (MonadReader env m, MonadIO m, HasReadFileFn env ReadFile)
    => Text
    -> m Text
readFile path = do
    env      <- ask
    contents <- liftIO $ (env ^. readFileFn) (T.unpack path)
    return . T.strip $ contents

exec :: (MonadReader env m, MonadIO m, HasExecFn env Execute) => Text -> m Text
exec command = do
    env <- ask
    -- TODO this is an incomplete match :(
    let (cmd : args) = T.splitOn " " command
    results <- liftIO $ (env ^. execFn) (T.unpack cmd) (T.unpack <$> args) []
    return . T.pack $ results
