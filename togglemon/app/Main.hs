module Main where

import           Control.Lens         ((^.))
import           Control.Monad.Reader (ask, runReaderT)
import qualified Data.ByteString      as BS
import           Data.Maybe           (catMaybes)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified System.Directory     as Dir
import qualified System.Process       as Proc
import           ToggleMon.Config     (InternalConfig)
import qualified ToggleMon.Config     as Config
import           ToggleMon.Display    (Display, DisplayConfiguration (..))
import qualified ToggleMon.Display    as Display
import qualified ToggleMon.IO         as ToggleIO
import           ToggleMon.Monad

getDisplays :: ToggleMon [Display]
getDisplays = do
    env     <- ask
    rawDirs <- ToggleIO.listDirectory $ env ^. displayBasePath
    dirs    <- case rawDirs of
        Nothing -> fail $ "Could not list directory contents for " ++ T.unpack
            (env ^. displayBasePath)
        Just r -> ToggleIO.filterDirectory r
    catMaybes <$> mapM Display.toDisplay dirs

updateConfig :: [Display] -> ToggleMon (Maybe InternalConfig)
updateConfig _ = do
    currentConfig <- Config.readConfig
    -- TODO real updateConfig functionality
    -- 1. Get current config
    -- 2. Update config with displays
    -- 3. Write config
    return $ case currentConfig of
        Left  _ -> Nothing
        Right v -> Just v

-- TODO this doesn't need to live in the ToggleMon monad. Needs refactoring.
getDisplayConfiguration :: [Display] -> ToggleMon (Maybe DisplayConfiguration)
getDisplayConfiguration displays =
    return
        $   Display.getActiveDisplay displays
        >>= \activeDisplay ->
                return $ case Display.getDisabledDisplay displays of
                    Nothing -> Single
                        activeDisplay
                        (filter (/= activeDisplay) displays)
                    Just disabledDisplay ->
                        ActivePassive activeDisplay disabledDisplay

environment :: Env
environment = Env
    { envDisplayBasePath  = defaultDisplayBasePath
    , envListDirFn        = Dir.listDirectory
    , envReadFileFn       = TIO.readFile
    , envExecFn           = Proc.readProcess
    , envReadByteStringFn = BS.readFile
    }

main :: IO ()
main =
    flip runReaderT environment
        $ runToggleMon
        $ do
              -- TODO centralize error handling, probably with an Either or some such
              displays      <- getDisplays
              displayConfig <- getDisplayConfiguration displays
              commandOutput <- mapM
                  (ToggleIO.exec . Display.buildXrandrCommand)
                  displayConfig

              case commandOutput of
                  Nothing -> fail "Failed to execute command"
                  Just _  -> return ()
