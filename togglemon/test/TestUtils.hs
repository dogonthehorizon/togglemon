module TestUtils where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Text           (Text)
import           System.FilePath     (FilePath)

singleDisplay :: Text -> Text -> HashMap FilePath Text
singleDisplay status enabled =
    Map.fromList [("status", status), ("enabled", enabled)]

displays :: HashMap FilePath (HashMap FilePath Text)
displays = Map.fromList
    [ ("video0-eDP-1", singleDisplay "connected" "enabled")
    , ("video0-DP-1" , singleDisplay "connected" "disabled")
    , ("video0-DP-2" , singleDisplay "connected" "disabled")
    ]
