{-|
Module : ToggleMon.IO
Description : Domain-specific IO functions for ToggleMon.
Copyright : (c) Fernando Freire, 2019
License : MIT
Maintainer : Fernando Freire
Stability : experimental

This module provides IO functionality specialized to the ToggleMon domain.
Chiefly they depend on some monadic context that provides the base IO functions
in question and wraps them in exception handling logic and conversions
between 'String' and 'Data.Text.Text' types.
-}
module ToggleMon.IO where

import           Control.Exception      (SomeException (..), try)
import           Control.Lens           ((^.))
import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified System.Directory       as Dir
import           ToggleMon.Monad

-- | List the contents of the given directory.
--
-- __NOTE:__ Failure cause is not currently captured, 'Either' values returned
-- from use of 'try' are lifted to a 'Maybe' and the left-hand side ignored.
-- Future updates may report the cause of failure (it'd be nice to have, but
-- complicates how this fn is used).
listDirectory
    :: (MonadReader env m, MonadIO m, HasListDirFn env ListDirectoryAction)
    => Text -- ^ The directory to list.
    -> m (Maybe [Text]) -- ^ 'Nothing' if some exception occurred, 'Just' the directory contents otherwise.
listDirectory path = do
    env               <- ask
    directoryContents <- liftIO . try $ (env ^. listDirFn) (T.unpack path)
    return $ either (\(SomeException _) -> Nothing) Just $ (fmap . fmap)
        T.pack
        directoryContents

-- | Filter a list of directory contents depending on whether the contents
-- themselves are directories.
--
-- __ TODO __ Dir.doesDirectoryExist needs to be lifted into the environment
--            record.
--
-- __ TODO __ it feels limiting to only be able to filter based off of the
--            displayBasePath; consider shifting this concern to the call site instead.
filterDirectory
    :: (MonadReader env m, MonadIO m, HasDisplayBasePath env Text)
    => [Text]   -- ^ Directory contents to filter
    -> m [Text] -- ^ Directory contents, with all non-directories excluded.
filterDirectory dirs = do
    env <- ask
    let qualifiedPath p = T.unpack (env ^. displayBasePath) <> T.unpack p
    liftIO $ filterM (Dir.doesDirectoryExist . qualifiedPath) dirs

-- | Read the given file and return it's contents.
--
-- __ TODO __ must handle IOException
readFile
    :: (MonadReader env m, MonadIO m, HasReadFileFn env ReadFileAction)
    => Text   -- ^ The file to read
    -> m Text -- ^ The file's contents
readFile path = do
    env      <- ask
    contents <- liftIO $ (env ^. readFileFn) (T.unpack path)
    return . T.strip $ contents

readByteString
    :: ( MonadReader env m
       , MonadIO m
       , HasReadByteStringFn env ReadByteStringAction
       )
    => Text -- ^ The file to read
    -> m ByteString -- ^ The file's contents as a ByteString
readByteString path = do
    env <- ask
    liftIO $ (env ^. readByteStringFn) (T.unpack path)

-- | Execute the given command and return it's result.
--
-- __ TODO __ should report IO failure
exec
    :: (MonadReader env m, MonadIO m, HasExecFn env ExecuteAction)
    => Text           -- ^ The full command to run (along with it's arguments)
    -> m (Maybe Text) -- ^ The output from executing this command
exec command = do
    env <- ask
    let (cmd : args) = T.splitOn " " command
    results <- liftIO . try $ (env ^. execFn)
        (T.unpack cmd)
        (T.unpack <$> args)
        []
    return . either (\(SomeException _) -> Nothing) Just $ T.pack <$> results
