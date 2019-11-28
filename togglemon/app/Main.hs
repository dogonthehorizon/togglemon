module Main where

import           Control.Error.Util   (hush)
import           Control.Lens         ((^.))
import           Control.Monad.Reader (ask, runReaderT)
import           Control.Monad.Trans  (liftIO)
import qualified Data.ByteString      as BS
import           Data.Maybe           (catMaybes, fromMaybe)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified System.Directory     as Dir
import qualified System.Process       as Proc
import qualified ToggleMon.Config     as Config
import           ToggleMon.Display    (Display)
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

-- TODO this return type needs to change, not currently doing anything with the Maybe
updateConfig :: [Display] -> ToggleMon (Maybe ())
updateConfig displays = do
    currentConfig <- Config.readConfig
    let cfg = fromMaybe Config.defaultInternalConfig (hush currentConfig)
    updatedConfig <- Config.updateConfig cfg displays
    liftIO . sequence $ Config.writeConfig <$> updatedConfig

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
              displays <- getDisplays
              _        <- updateConfig displays

              let
                  cmd = Display.buildXrandrCommand
                      <$> Display.getDisplayConfiguration displays

              commandOutput <- mapM ToggleIO.exec cmd

              case commandOutput of
                  Nothing      -> fail "Failed to execute command"
                  Just Nothing -> fail "Failed to execute command"
                  Just _       -> return ()
