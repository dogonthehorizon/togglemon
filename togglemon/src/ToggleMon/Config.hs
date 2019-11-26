module ToggleMon.Config where

import           Control.Arrow                  (left)
import           Control.Monad.Trans            (MonadIO, liftIO)
import           Control.Retry                  (limitRetries, retrying)
import           Data.Edid.Types                (Edid, EdidVersion,
                                                 Manufacturer)
import qualified Data.Edid.Types                as Edid
import           Data.Foldable                  (foldlM)
import           Data.List                      (find)
import           Data.Maybe                     (fromMaybe, isNothing)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Yaml                      (FromJSON, ToJSON,
                                                 decodeFileEither, encodeFile)
import           GHC.Generics                   (Generic)
import           MemorableName                  (MemorableName, memorableName)
import           System.Environment.XDG.BaseDir (getUserDataDir)
import           System.FilePath                ((</>))
import           ToggleMon.Display              (Display (..))

type Name = Text

instance ToJSON Edid
instance FromJSON Edid
instance ToJSON Manufacturer
instance FromJSON Manufacturer
instance ToJSON EdidVersion
instance FromJSON EdidVersion

data HardwareDisplay = HardwareDisplay {
    name :: Text,
    edid :: Edid
  } deriving (Show, Generic)

defaultDisplay :: HardwareDisplay
defaultDisplay = HardwareDisplay "default" Edid.empty

instance ToJSON HardwareDisplay
instance FromJSON HardwareDisplay

data InternalConfig = InternalConfig {
    knownDisplays :: [HardwareDisplay]
  } deriving (Show, Generic)

defaultInternalConfig :: InternalConfig
defaultInternalConfig = InternalConfig []

instance ToJSON InternalConfig
instance FromJSON InternalConfig

data ToggleMonConfig = ToggleMonConfig {
    displayConfig :: [HardwareDisplay]
  }

findInConfig :: InternalConfig -> Display -> Maybe HardwareDisplay
findInConfig cfg (Display _ _ _ displayEdid) = find
    (\(HardwareDisplay _ configEdid) -> displayEdid == configEdid)
    (knownDisplays cfg)

writeConfig :: InternalConfig -> IO ()
writeConfig config = do
    dataDir <- getUserDataDir "togglemon"
    -- TODO support when xdg-data-dir doesn't exist
    encodeFile (dataDir </> "known-displays.yaml") config

-- TODO better handle Either type
readConfig :: MonadIO m => m (Either String InternalConfig)
readConfig = do
    dataDir <- liftIO $ getUserDataDir "togglemon"
    content <- liftIO $ decodeFileEither (dataDir </> "known-displays.yaml")
    return $ left show content

generateName :: MonadIO m => InternalConfig -> m (Maybe MemorableName)
generateName cfg =
    retrying (limitRetries 100) (const $ return . isNothing) $ \_ -> do
        memName <- memorableName
        return $ if show memName `elem` (show . name <$> knownDisplays cfg)
            then Nothing
            else Just memName

-- TODO this feels pretty ugly.
updateConfig'
    :: MonadIO m => InternalConfig -> Display -> m (Maybe InternalConfig)
updateConfig' cfg display@(Display _ _ _ edid) =
    case findInConfig cfg display of
        Just _  -> return . Just $ cfg
        Nothing -> do
            generated <- generateName cfg
            return $ generated >>= \name ->
                let
                    hardwareDisplay =
                        HardwareDisplay (T.pack . show $ name) edid
                in
                    return InternalConfig
                        { knownDisplays = hardwareDisplay : knownDisplays cfg
                        }

-- TODO refactor this, not happy with fromMaybe/Just shenanigans.
updateConfig
    :: MonadIO m => InternalConfig -> [Display] -> m (Maybe InternalConfig)
updateConfig cfg = foldlM
    (\currentConfig display ->
        updateConfig' (fromMaybe cfg currentConfig) display
    )
    (Just cfg)
