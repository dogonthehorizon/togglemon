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
data DisplayConfiguration =
  ActivePassive
    Display -- ^ The currently active display.
    Display -- ^ The connected, passive display.
  | Single Display [Display]
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

    case displayContents of
        Nothing -> return Nothing
        Just contents ->
            if "status" `elem` contents && "enabled" `elem` contents
                then do
                    status <- ToggleIO.readFile
                        $ T.concat [displayPath, "/", "status"]
                    enabled <- ToggleIO.readFile
                        $ T.concat [displayPath, "/", "enabled"]

                    return
                        $   Display dName
                        <$> toEnabled enabled
                        <*> toStatus status
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

disableDisplay :: Display -> [Text]
disableDisplay (Display name _ _) =
    ["--output", toXrandrDisplayName name, "--off"]

enableDisplay :: Display -> [Text]
enableDisplay (Display name _ _) =
    [ "--output"
    , toXrandrDisplayName name
    , "--pos"
    , "0x0"
    , "--auto"
    , "--scale"
    , "2x2"
    ]

render :: [Text] -> Text
render = T.intercalate " "

-- TODO support setting scaling options per display
buildXrandrCommand :: DisplayConfiguration -> Text
buildXrandrCommand (ActivePassive activeDisplay disabledDisplay) =
    T.strip $ "xrandr " <> render (disableDisplay activeDisplay) <> render
        (enableDisplay disabledDisplay)

buildXrandrCommand (Single activeDisplay displays) =
    T.strip
        $  "xrandr "
        <> render (render . disableDisplay <$> displays)
        <> " "
        <> render (enableDisplay activeDisplay)
