-- TODO module header explaining purpose. Could also use some general cleanup.
module TestUtils where

import Prelude hiding (readFile)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Text           (Text)
import qualified Data.Text as T
import           System.FilePath     (FilePath)
import ToggleMon.Monad (Env(..), ToggleMon(..))
import           Control.Monad.Reader (runReaderT)

singleDisplay :: Text -> Text -> HashMap FilePath Text
singleDisplay status enabled =
    Map.fromList [("status", status), ("enabled", enabled)]

displays :: HashMap FilePath (HashMap FilePath Text)
displays = Map.fromList
    [ ("video0-eDP-1", singleDisplay "connected" "enabled")
    , ("video0-DP-1" , singleDisplay "connected" "disabled")
    , ("video0-DP-2" , singleDisplay "disconnected" "disabled")
    ]

listDirectory :: FilePath -> IO [FilePath]
listDirectory fp
  | fp == "root" = return . Map.keys $ displays
  | otherwise  = return . Map.keys $ Map.lookupDefault Map.empty fp displays

readFile :: FilePath -> IO Text
readFile fp =
  let (_ : displayName : fileName : _) = T.splitOn "/" . T.pack $ fp
  in
    return $ Map.lookupDefault "" (T.unpack fileName) $
      Map.lookupDefault Map.empty (T.unpack displayName) $
        displays

mockEnv :: Env
mockEnv = Env {
  envDisplayBasePath = "root",
  envListDirFn = listDirectory,
  envReadFileFn = readFile,
  envExecFn = undefined -- TODO
}

runTestMonad :: ToggleMon a -> IO a
runTestMonad fn = runReaderT (runToggleMon fn) mockEnv
