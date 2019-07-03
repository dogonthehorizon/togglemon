module Main where

import           Control.Lens           ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask, runReaderT)
import           Data.Maybe             (catMaybes)
import qualified Data.Text.IO           as TIO
import qualified System.Directory       as Dir
import           ToggleMon.Display
import           ToggleMon.IO
import           ToggleMon.Monad

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
