{-|
Module : ToggleMon.Display
Description : Functions and types for managing DRM displays.
Copyright : (c) Fernando Freire, 2019
Maintainer : Fernando Freire
Stability : stable
-}
module ToggleMon.Display where

import           Control.Error.Util     (hush)
import           Control.Lens           ((^.))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ask)
import qualified Data.Edid.Parser       as Edid
import           Data.Edid.Types        (Edid (..))
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
data Display = Display DisplayName Enabled Status Edid deriving (Show, Eq)

-- | Configured display configurations.
data DisplayConfiguration =
  ActivePassive -- ^ Display configuration for toggling between active and passive displays.
    Display -- ^ The currently active display.
    Display -- ^ The connected, passive display.
  | Single -- ^ Display configuration when only one display is active.
      Display -- ^ The only active display.
      [Display] -- ^ All other potential displays for the machine.
    deriving (Show, Eq)

-- | Construct a 'Status'.
toStatus :: Text -> Maybe Status
toStatus s = case s of
    "connected"    -> Just Connected
    "disconnected" -> Just Disconnected
    _              -> Nothing

-- | Construct an 'Enabled' value.
toEnabled :: Text -> Maybe Enabled
toEnabled s = case s of
    "enabled"  -> Just Enabled
    "disabled" -> Just Disabled
    _          -> Nothing

-- | Construct a 'Display' given the information in /sys/class/drm.
toDisplay
    :: ( MonadReader env m
       , MonadIO m
       , HasReadFileFn env ReadFileAction
       , HasReadByteStringFn env ReadByteStringAction
       , HasListDirFn env ListDirectoryAction
       , HasDisplayBasePath env Text
       )
    => DisplayName
    -> m (Maybe Display)
toDisplay dName = do
    env <- ask

    let displayPath = T.concat [env ^. displayBasePath, dName]

    displayContents <- ToggleIO.listDirectory displayPath

    case displayContents of
        Nothing -> return Nothing
        Just contents ->
            if "status" `elem` contents && "enabled" `elem` contents
                then do
                    status <- ToggleIO.readFile
                        $ T.concat [displayPath, "/", "status"]
                    enabled <- ToggleIO.readFile
                        $ T.concat [displayPath, "/", "enabled"]
                    edidFile <- ToggleIO.readByteString
                        $ T.concat [displayPath, "/", "edid"]

                    return
                        $   Display dName
                        <$> toEnabled enabled
                        <*> toStatus status
                        -- TODO properly handle the Either value
                        <*> hush (Edid.parseContent edidFile)
                else return Nothing

-- | Get the first display that is in a enabled _and_ connected state.
getActiveDisplay :: [Display] -> Maybe Display
getActiveDisplay = find activeDisplay
  where
    activeDisplay (Display _ Enabled Connected _) = True
    activeDisplay _                               = False

-- | Get the first display that is in a disabled but connected state.
getDisabledDisplay :: [Display] -> Maybe Display
getDisabledDisplay = find disabledDisplay
  where
    disabledDisplay (Display _ Disabled Connected _) = True
    disabledDisplay _ = False

-- | Construct an `xrandr` compatible display name.
toXrandrDisplayName :: Text -> Text
toXrandrDisplayName = T.concat . drop 1 . T.splitOn "-"

-- | Construct an `xrandr` command segment to disable the given display.
disableDisplay :: Display -> [Text]
disableDisplay (Display name _ _ _) =
    ["--output", toXrandrDisplayName name, "--off"]

-- | Construct an `xrandr` command segment to enable the given display.
enableDisplay :: Display -> [Text]
enableDisplay (Display name _ _ _) =
    [ "--output"
    , toXrandrDisplayName name
    , "--pos"
    , "0x0"
    , "--auto"
    , "--scale"
    , "2x2"
    ]

-- | Render a command segment into a single string.
render :: [Text] -> Text
render = T.intercalate " "

-- | Build a correct `xrandr` command to be executed somewhere.
-- TODO support setting scaling options per display
buildXrandrCommand :: DisplayConfiguration -> Text
buildXrandrCommand (ActivePassive activeDisplay disabledDisplay) =
    T.strip $ render ("xrandr" : disableDisplay activeDisplay) <> render
        (enableDisplay disabledDisplay)
buildXrandrCommand (Single activeDisplay displays) =
    T.strip
        $  "xrandr"
        <> render (render . disableDisplay <$> displays)
        <> " "
        <> render (enableDisplay activeDisplay)
