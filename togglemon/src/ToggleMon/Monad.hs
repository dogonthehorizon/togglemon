-- TODO module header && fn docstrings
module ToggleMon.Monad where

import Control.Lens           (camelCaseFields, makeLensesWith)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (MonadReader, ReaderT)
import Data.Text              (Text)
import System.FilePath        (FilePath)

type ReadFileAction      = FilePath -> IO Text
type ListDirectoryAction = FilePath -> IO [FilePath]
type ExecuteAction       = FilePath -> [String] -> String -> IO String

data Env = Env {
  envDisplayBasePath :: Text,
  envListDirFn       :: !ListDirectoryAction,
  envReadFileFn      :: !ReadFileAction,
  envExecFn          :: !ExecuteAction
}

makeLensesWith camelCaseFields ''Env

newtype ToggleMon a = ToggleMon {
  runToggleMon :: ReaderT Env IO a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

