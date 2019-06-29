module Main where

import Prelude hiding (readFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as Dir
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Lens (makeLensesWith, camelCaseFields, (^.))
import System.FilePath (FilePath)

type ReadFile = FilePath -> IO Text
type ListDirectory = FilePath -> IO [FilePath]

data Env = Env {
  envDisplayBasePath :: Text,
  envListDirFn :: !ListDirectory,
  envReadFileFn :: !ReadFile
}

makeLensesWith camelCaseFields ''Env

newtype ToggleMon a = ToggleMon {
  runToggleMon :: ReaderT Env IO a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

data Status = Connected | Disconnected deriving (Show, Eq)
data Enabled = Enabled | Disabled deriving (Show, Eq)
type DisplayName = Text
data Display = Display DisplayName Enabled Status deriving (Show, Eq)

-- TODO this is filtering everything, instead of just "version". Woops.
listDirectory :: (MonadReader env m,
                    MonadIO m,
                    HasListDirFn env ListDirectory) => Text -> m [Text]
listDirectory path = do
  env <- ask
  rawDirs <- liftIO $ (env ^. listDirFn) (T.unpack path)
  dirs <- liftIO $ filterM Dir.doesDirectoryExist $ rawDirs
  return . fmap T.pack $ dirs

readFile :: (MonadReader env m,
             MonadIO m,
             HasReadFileFn env ReadFile) => Text -> m Text
readFile path = do
  env <- ask
  contents <- liftIO $ (env ^. readFileFn) (T.unpack path)
  return . T.strip $ contents

toStatus :: Text -> Status
toStatus s =
  case s of
    "connected" -> Connected
    "disconnected" -> Disconnected
    otherwise -> error "shouldn't happen"

toEnabled :: Text -> Enabled
toEnabled s =
  case s of
    "enabled" -> Enabled
    "disabled" -> Disabled
    otherwise -> error "shouldn't happen"

toDisplay ::
  (MonadReader env m,
   MonadIO m,
   HasReadFileFn env ReadFile,
   HasListDirFn env ListDirectory,
   HasDisplayBasePath env Text) =>
  DisplayName -> m (Maybe Display)
toDisplay dName = do
  env <- ask

  let displayPath = T.concat [(env ^. displayBasePath), dName]

  displayContents <- listDirectory $ displayPath

  if "status" `elem` displayContents && "enabled" `elem` displayContents
     then do
       status <- readFile $ T.concat [displayPath, "/", "status"]
       enabled <- readFile $ T.concat [displayPath, "/", "enabled"]

       return . Just $ Display dName (toEnabled enabled) (toStatus status)
     else return Nothing

run :: ToggleMon ()
run = do
  env <- ask
  dirs <- listDirectory $ env ^. displayBasePath
  liftIO $ print dirs

  (flip mapM_) dirs $ \dName -> do
    liftIO $ print dName

    display <- toDisplay dName

    liftIO $ print display

main :: IO ()
main = do
  let env = Env {
    envDisplayBasePath = "/sys/class/drm/",
    envListDirFn = Dir.listDirectory,
    envReadFileFn = TIO.readFile
  }

  runReaderT (runToggleMon run) env
