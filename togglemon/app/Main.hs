module Main where

import           Control.Lens         ((^.))
import           Control.Monad.Reader (ask, runReaderT)
import           Data.Maybe           (catMaybes)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified System.Directory     as Dir
import qualified System.Process       as Proc
import           ToggleMon.Display
import           ToggleMon.IO
import           ToggleMon.Monad

getDisplayConfiguration :: ToggleMon (Maybe DisplayConfiguration)
getDisplayConfiguration = do
    env     <- ask
    rawDirs <- listDirectory $ env ^. displayBasePath
    dirs    <- case rawDirs of
        Nothing -> fail $ "Could not list directory contents for " ++ T.unpack
            (env ^. displayBasePath)
        Just r -> filterDirectory r
    displays <- catMaybes <$> mapM toDisplay dirs

    return
        $   getActiveDisplay displays
        >>= \activeDisplay -> return $ case getDisabledDisplay displays of
                Nothing ->
                    Single activeDisplay (filter (/= activeDisplay) displays)
                Just disabledDisplay ->
                    ActivePassive activeDisplay disabledDisplay

-- TODO centralize error handling, probably with an Either or some such
run :: ToggleMon ()
run = do
    displayConfig <- getDisplayConfiguration
    commandOutput <- mapM (exec . buildXrandrCommand) displayConfig

    case commandOutput of
        Nothing -> fail "Failed to execute command"
        Just _  -> return ()

environment :: Env
environment = Env
    { envDisplayBasePath = defaultDisplayBasePath
    , envListDirFn       = Dir.listDirectory
    , envReadFileFn      = TIO.readFile
    , envExecFn          = Proc.readProcess
    }

main :: IO ()
main = runReaderT (runToggleMon run) environment
