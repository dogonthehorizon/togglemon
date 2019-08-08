module Main where

import           Control.Lens         ((^.))
import           Control.Monad.Reader (ask, runReaderT)
import           Data.Maybe           (catMaybes)
import qualified Data.Text.IO         as TIO
import qualified System.Directory     as Dir
import qualified System.Process       as Proc
import           ToggleMon.Display
import           ToggleMon.IO
import           ToggleMon.Monad

run :: ToggleMon ()
run = do
    env      <- ask
    rawDirs  <- listDirectory $ env ^. displayBasePath
    dirs     <- filterDirectory rawDirs
    displays <- catMaybes <$> mapM toDisplay dirs

    mapM_ (exec . buildXrandrCommand)
        $   ActivePassiveDisplayConfiguration
        <$> getActiveDisplay displays
        <*> getDisabledDisplay displays


main :: IO ()
main = do
    let
        env = Env
            { envDisplayBasePath = "/sys/class/drm/"
            , envListDirFn       = Dir.listDirectory
            , envReadFileFn      = TIO.readFile
            , envExecFn          = Proc.readProcess
            }

    runReaderT (runToggleMon run) env
