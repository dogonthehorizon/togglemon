{-|
Module : ToggleMon.Display
Description : Functions and types for managing DRM displays.
Copyright : (c) Fernando Freire, 2019
Maintainer : Fernando Freire
Stability : stable
-}
module ToggleMon.Display where

import           Control.Lens           ((^.))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.List              (find)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified ToggleMon.IO           as ToggleIO
import           ToggleMon.Monad

-- | The connection status of a display.
data Status = Connected | Disconnected deriving (Show, Eq)

-- | The usage status of a display.
data Enabled = Enabled | Disabled deriving (Show, Eq)

-- | 'Text' alias for a display's name.
type DisplayName = Text

-- | Representation of a single DRM display.
data Display = Display DisplayName Enabled Status deriving (Show, Eq)

-- | Represenation of an active/passive display setup.
--
-- This is the originaly configuration strategy for togglemon. The first
-- display represents the currently active display while the second represents
-- a passive display that is connected. When triggered, the passive display
-- will be configured and swapped with the active display.
data ActivePassiveDisplayConfiguration =
  ActivePassiveDisplayConfiguration
    Display -- ^ The currently active display.
    Display -- ^ The connected, passive display.
    deriving (Show, Eq)

-- | Construct a 'Status'.
--
-- TODO convert to total function.
toStatus :: Text -> Status
toStatus s = case s of
    "connected"    -> Connected
    "disconnected" -> Disconnected
    _              -> error "shouldn't happen"

-- | Construct an 'Enabled' value.
--
-- TODO convert to total function.
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
buildXrandrCommand :: ActivePassiveDisplayConfiguration -> Text
buildXrandrCommand (ActivePassiveDisplayConfiguration (Display activeName _ _) (Display disabledName _ _))
    = "xrandr --output "
        <> toXrandrDisplayName activeName
        <> " --off --output "
        <> toXrandrDisplayName disabledName
        <> " --pos 0x0 --auto --scale 2x2"
