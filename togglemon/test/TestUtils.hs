-- TODO module header explaining purpose. Could also use some general cleanup.
module TestUtils where

import           Control.Monad.Reader (runReaderT)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Prelude              hiding (readFile)
import           System.FilePath      (FilePath)
import           ToggleMon.Monad      (Env (..), ToggleMon (..))

singleDisplay :: Text -> Text -> HashMap FilePath Text
singleDisplay status enabled =
    Map.fromList [("status", status), ("enabled", enabled)]

displays :: HashMap FilePath (HashMap FilePath Text)
displays = Map.fromList
    [ ("card0-eDP-1", singleDisplay "connected" "enabled")
    , ("card0-DP-1" , singleDisplay "connected" "disabled")
    , ("card0-DP-2" , singleDisplay "disconnected" "disabled")
    ]

listDirectory :: FilePath -> IO [FilePath]
listDirectory fp
    | fp == "root" = return . Map.keys $ displays
    | otherwise = return . Map.keys $ Map.lookupDefault
        Map.empty
        (pathSegments fp !! 1)
        displays

pathSegments :: FilePath -> [FilePath]
pathSegments = fmap T.unpack . T.splitOn "/" . T.pack

readFile :: FilePath -> IO Text
readFile fp =
    let (_ : displayName : fileName : _) = pathSegments fp
    in
        return $ Map.lookupDefault "" fileName $ Map.lookupDefault
            Map.empty
            displayName
            displays

mockEnv :: Env
mockEnv = Env
    { envDisplayBasePath = "root/"
    , envListDirFn       = listDirectory
    , envReadFileFn      = readFile
    , envExecFn          = \_ _ _ -> return "" -- TODO
    }

runTestMonad :: ToggleMon a -> IO a
runTestMonad fn = runReaderT (runToggleMon fn) mockEnv
