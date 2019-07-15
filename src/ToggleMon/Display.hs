-- TODO module header && fn docstrings
module ToggleMon.Display where

import           Control.Lens           ((^.))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.List              (find)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified ToggleMon.IO           as ToggleIO
import           ToggleMon.Monad

data Status = Connected | Disconnected deriving (Show, Eq)
data Enabled = Enabled | Disabled deriving (Show, Eq)
type DisplayName = Text
data Display = Display DisplayName Enabled Status deriving (Show, Eq)

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
       , HasReadFileFn env ReadFileAction
       , HasListDirFn env ListDirectoryAction
       , HasDisplayBasePath env Text
       )
    => DisplayName
    -> m (Maybe Display)
toDisplay dName = do
    env <- ask

    let displayPath = T.concat [env ^. displayBasePath, dName]

    displayContents <- ToggleIO.listDirectory displayPath

    if "status" `elem` displayContents && "enabled" `elem` displayContents
        then do
            status  <- ToggleIO.readFile $ T.concat [displayPath, "/", "status"]
            enabled <- ToggleIO.readFile
                $ T.concat [displayPath, "/", "enabled"]

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

-- TODO support setting scaling options per display
buildXrandrCommand :: DisplaySetup -> Text
buildXrandrCommand (DisplaySetup (Display activeName _ _) (Display disabledName _ _))
    = "xrandr --output "
        <> toXrandrDisplayName activeName
        <> " --off --output "
        <> toXrandrDisplayName disabledName
        <> " --pos 0x0 --auto --scale 2x2"
