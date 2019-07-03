module ToggleMon.Monad where

import Control.Lens           (camelCaseFields, makeLensesWith)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (MonadReader, ReaderT)
import Data.Text              (Text)
import System.FilePath        (FilePath)

type ReadFile = FilePath -> IO Text
type ListDirectory = FilePath -> IO [FilePath]

data Env = Env {
  envDisplayBasePath :: Text,
  envListDirFn       :: !ListDirectory,
  envReadFileFn      :: !ReadFile
}

makeLensesWith camelCaseFields ''Env

newtype ToggleMon a = ToggleMon {
  runToggleMon :: ReaderT Env IO a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

