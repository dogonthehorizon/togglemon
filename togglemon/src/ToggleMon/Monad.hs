{-|
Module : ToggleMon.Monad
Description : Domain-specific Monad for ToggleMon (and related boilerplate).
Copyright : (c) Fernando Freire, 2019
License : MIT
Maintainer : Fernando Freire
Stability : experimental

This module provides a Monad for ToggleMon (and related boilerplate).
-}
module ToggleMon.Monad where

import Control.Lens           (camelCaseFields, makeLensesWith)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (MonadReader, ReaderT)
import Data.Text              (Text)
import System.FilePath        (FilePath)

-- | Action representing reading a file within the IO monad.
type ReadFileAction      = FilePath -> IO Text
-- | Action representing listing the contents of a directory within the IO
-- monad.
type ListDirectoryAction = FilePath -> IO [FilePath]
-- | Action representing exeucting some command with some list of arguments
-- within the IO monad.
type ExecuteAction       = FilePath -> [String] -> String -> IO String

-- | The environment for 'ToggleMon' containing side-effecting functions and
-- configuration.
data Env = Env {
  envDisplayBasePath :: Text,
  -- ^ The location in a filesystem where Linux DRM information is exposed.
  envListDirFn       :: !ListDirectoryAction,
  envReadFileFn      :: !ReadFileAction,
  envExecFn          :: !ExecuteAction
}

-- | Most common location where Linux DRM information will be exposed.
defaultDisplayBasePath :: Text
defaultDisplayBasePath = "/sys/class/drm/"

makeLensesWith camelCaseFields ''Env

-- | The ToggleMon monad consisting of a reader context and IO.
newtype ToggleMon a = ToggleMon {
  runToggleMon :: ReaderT Env IO a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

