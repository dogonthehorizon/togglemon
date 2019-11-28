-- TODO module header explaining purpose. Could also use some general cleanup.
module TestUtils where

import           Control.Exception    (Exception, throw)
import           Control.Monad.Reader (runReaderT)
import           Data.ByteString      (ByteString)
import           Data.Edid            (Edid (..), EdidVersion (..),
                                       Manufacturer (..))
import qualified Data.Edid            as Edid
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List            (intercalate)
import           Data.String          (IsString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Prelude              hiding (readFile)
import           System.FilePath      (FilePath)
import           ToggleMon.Display    (Display (..), Enabled (..), Status (..))
import           ToggleMon.Monad      (Env (..), ExecuteAction, ToggleMon (..))

safeHead :: [a] -> Maybe a
safeHead xs = if not (null xs) then Just $ head xs else Nothing

defaultDisplay :: Display
defaultDisplay = Display "" Disabled Disconnected sampleEdid

singleDisplay :: Text -> Text -> HashMap FilePath Text
singleDisplay status enabled =
    Map.fromList [("status", status), ("enabled", enabled)]

displays :: HashMap FilePath (HashMap FilePath Text)
displays = Map.fromList
    [ ("card0-eDP-1"       , singleDisplay "connected" "enabled")
    , ("card0-DP-1"        , singleDisplay "connected" "disabled")
    , ("card0-DP-2"        , singleDisplay "disconnected" "disabled")
    , ("card0-DP-bad-data" , singleDisplay "foo" "bar")
    , ("card0-DP-empty-dir", Map.empty)
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

sampleEdid :: Edid
sampleEdid = Edid
    { manufacturer      = Manufacturer "SHP" 5293
    , serialNumber      = 0
    , weekOfManufacture = 42
    , yearOfManufacture = 2018
    , version           = V1_4
    }

sampleEdidContents :: ByteString
sampleEdidContents
    = "\NUL\255\255\255\255\255\255\NULM\DLE\173\DC4\NUL\NUL\NUL\NUL*\FS\SOH\EOT\165\GS\DC1x\SO\222P\163TL\153&\SIPT\NUL\NUL\NUL\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOHM\208\NUL\160\240p>\128\&0 5\NUL&\165\DLE\NUL\NUL\CAN\164\166\NUL\160\240p>\128\&0 5\NUL&\165\DLE\NUL\NUL\CAN\NUL\NUL\NUL\254\NUL0R99K\128LQ133D1\NUL\NUL\NUL\NUL\NUL\STXA\ETX(\SOH\DC2\NUL\NUL\v\SOH\n  \NULA"

-- | Always returns the same edid because reasons
--
-- Maybe when the `edid` package supports serialization we can take advantage
-- of this better.
readFileBs :: FilePath -> IO ByteString
readFileBs _ = return sampleEdidContents

data ExecException = ExecException deriving Show
instance Exception ExecException

exec :: ExecuteAction
exec "fail" _    _ = throw ExecException
exec cmd    args _ = return $ intercalate "_" (cmd : args)


mockEnv :: Env
mockEnv = Env
    { envDisplayBasePath  = "root/"
    , envListDirFn        = listDirectory
    , envReadFileFn       = readFile
    , envExecFn           = exec
    , envReadByteStringFn = readFileBs
    }

runTestMonad :: ToggleMon a -> IO a
runTestMonad fn = runReaderT (runToggleMon fn) mockEnv

runTestMonad' :: Env -> ToggleMon a -> IO a
runTestMonad' e fn = runReaderT (runToggleMon fn) e
