module Main where

import           Control.Lens           (camelCaseFields, makeLensesWith, (^.))
import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Data.List              (find)
import           Data.Maybe             (catMaybes)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Prelude                hiding (readFile)
import qualified System.Directory       as Dir
import           System.FilePath        (FilePath)

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

data Status = Connected | Disconnected deriving (Show, Eq)
data Enabled = Enabled | Disabled deriving (Show, Eq)
type DisplayName = Text
data Display = Display DisplayName Enabled Status deriving (Show, Eq)

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

toStatus :: Text -> Status
toStatus s = case s of
    "connected"    -> Connected
    "disconnected" -> Disconnected
    _              -> error "shouldn't happen"

toEnabled :: Text -> Enabled
toEnabled s = case s of
    "enabled"  -> Enabled
    "disabled" -> Disabled
    _          -> error "shouldn't happen"

toDisplay
    :: ( MonadReader env m
       , MonadIO m
       , HasReadFileFn env ReadFile
       , HasListDirFn env ListDirectory
       , HasDisplayBasePath env Text
       )
    => DisplayName
    -> m (Maybe Display)
toDisplay dName = do
    env <- ask

    let displayPath = T.concat [env ^. displayBasePath, dName]

    displayContents <- listDirectory displayPath

    if "status" `elem` displayContents && "enabled" `elem` displayContents
        then do
            status  <- readFile $ T.concat [displayPath, "/", "status"]
            enabled <- readFile $ T.concat [displayPath, "/", "enabled"]

            return . Just $ Display dName (toEnabled enabled) (toStatus status)
        else return Nothing

data DisplaySetup = DisplaySetup Display Display deriving (Show, Eq)

getActiveDisplay :: [Display] -> Maybe Display
getActiveDisplay = find activeDisplay
  where
    activeDisplay (Display _ Enabled Connected) = True
    activeDisplay _                             = False

getDisabledDisplay :: [Display] -> Maybe Display
getDisabledDisplay = find disabledDisplay
  where
    disabledDisplay (Display _ Disabled Connected) = True
    disabledDisplay _                              = False

toXrandrDisplayName :: Text -> Text
toXrandrDisplayName = T.concat . drop 1 . T.splitOn "-"

buildXrandrCommand :: DisplaySetup -> Text
buildXrandrCommand (DisplaySetup (Display activeName _ _) (Display disabledName _ _))
    = "xrandr --output "
        <> toXrandrDisplayName activeName
        <> " --off --output "
        <> toXrandrDisplayName disabledName
        <> " --pos 0x0 --auto --scale 2x2"

run :: ToggleMon ()
run = do
    env      <- ask
    rawDirs  <- listDirectory $ env ^. displayBasePath
    dirs     <- filterDirectory rawDirs
    displays <- catMaybes <$> mapM toDisplay dirs

    (liftIO . print)
        $   buildXrandrCommand
        <$> (   DisplaySetup
            <$> getActiveDisplay displays
            <*> getDisabledDisplay displays
            )

main :: IO ()
main = do
    let
        env = Env
            { envDisplayBasePath = "/sys/class/drm/"
            , envListDirFn       = Dir.listDirectory
            , envReadFileFn      = TIO.readFile
            }

    runReaderT (runToggleMon run) env
