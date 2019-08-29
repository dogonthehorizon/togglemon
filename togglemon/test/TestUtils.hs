-- TODO module header explaining purpose. Could also use some general cleanup.
module TestUtils where

import           Control.Exception    (Exception, throw)
import           Control.Monad.Reader (runReaderT)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List            (intercalate)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Prelude              hiding (readFile)
import           System.FilePath      (FilePath)
import           ToggleMon.Display    (Display (..), Enabled (..), Status (..))
import           ToggleMon.Monad      (Env (..), ExecuteAction, ToggleMon (..))

safeHead :: [a] -> Maybe a
safeHead xs = if not (null xs) then Just $ head xs else Nothing

defaultDisplay :: Display
defaultDisplay = Display "" Disabled Disconnected

singleDisplay :: Text -> Text -> HashMap FilePath Text
singleDisplay status enabled =
    Map.fromList [("status", status), ("enabled", enabled)]

displays :: HashMap FilePath (HashMap FilePath Text)
displays = Map.fromList
    [ ("card0-eDP-1"      , singleDisplay "connected" "enabled")
    , ("card0-DP-1"       , singleDisplay "connected" "disabled")
    , ("card0-DP-2"       , singleDisplay "disconnected" "disabled")
    , ("card0-DP-bad-data", singleDisplay "foo" "bar")
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

data ExecException = ExecException deriving Show
instance Exception ExecException

exec :: ExecuteAction
exec "fail" _    _ = throw ExecException
exec cmd    args _ = return $ intercalate "_" (cmd : args)


mockEnv :: Env
mockEnv = Env
    { envDisplayBasePath = "root/"
    , envListDirFn       = listDirectory
    , envReadFileFn      = readFile
    , envExecFn          = exec
    }

runTestMonad :: ToggleMon a -> IO a
runTestMonad fn = runReaderT (runToggleMon fn) mockEnv

runTestMonad' :: Env -> ToggleMon a -> IO a
runTestMonad' e fn = runReaderT (runToggleMon fn) e
