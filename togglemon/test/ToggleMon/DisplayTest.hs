module ToggleMon.DisplayTest where

import           Data.Maybe                               (fromMaybe)
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Test.SmallCheck.Series.Instances         ()
import           Test.Tasty                               (localOption,
                                                           testGroup)
import           Test.Tasty.HUnit                         (testCase, (@?=))
import           Test.Tasty.SmallCheck                    (SmallCheckDepth (..),
                                                           testProperty)
import           TestUtils
import qualified ToggleMon.Display                        as Display
import           ToggleMon.Test.Smallcheck.Series.Display ()

test_module = testGroup
    "ToggleMon.Display"
    [ toStatus
    , toEnabled
    , toDisplay
    , getActiveDisplay
    , getDisabledDisplay
    , toXrandrDisplayName
    , buildXrandrCommand
    ]

toStatus = testGroup
    "toStatus"
    [ testCase "serialize connected"
    $   Display.toStatus "connected"
    @?= Just Display.Connected
    , testCase "serialize disconnected"
    $   Display.toStatus "disconnected"
    @?= Just Display.Disconnected
    , testCase "fail with garbage data"
    $   Display.toStatus "a;sdfasdf"
    @?= Nothing
    ]

toEnabled = testGroup
    "toEnabled"
    [ testCase "serialize enabled"
    $   Display.toEnabled "enabled"
    @?= Just Display.Enabled
    , testCase "serialize disabled"
    $   Display.toEnabled "disabled"
    @?= Just Display.Disabled
    , testCase "fail with garbage data"
    $   Display.toEnabled "a;sdfasdf"
    @?= Nothing
    ]

toDisplay = testGroup
    "toDisplay"
    [ testCase "should properly serialize a known monitor" $ do
        let
            expected =
                Display.Display "card0-eDP-1" Display.Enabled Display.Connected

        result <- runTestMonad $ Display.toDisplay "card0-eDP-1"

        fromMaybe defaultDisplay result @?= expected
    , testCase "should return nothing if display is not found" $ do
        result <- runTestMonad $ Display.toDisplay "does not exist"
        result @?= Nothing
    , testCase "should return nothing if status/enabled files have garbage" $ do
        result <- runTestMonad $ Display.toDisplay "card0-DP-bad-data"
        result @?= Nothing
    ]

getActiveDisplay = testGroup
    "getActiveDisplay"
    [ testProperty "should get the first active display if connected/enabled"
          $ \(displays :: [Display.Display]) ->
                let
                    enabledDisplays = filter
                        (\case
                            Display.Display _ Display.Enabled Display.Connected
                                -> True
                            _ -> False
                        )
                        displays
                in Display.getActiveDisplay displays == safeHead enabledDisplays
    ]

getDisabledDisplay = testGroup
    "getDisabledDisplay"
    [ testProperty "should get the first passive display if disabled/connected"
          $ \(displays :: [Display.Display]) ->
                let
                    disabledDisplays = filter
                        (\case
                            Display.Display _ Display.Disabled Display.Connected
                                -> True
                            _ -> False
                        )
                        displays
                in
                    Display.getDisabledDisplay displays
                        == safeHead disabledDisplays
    ]

toXrandrDisplayName = testGroup
    "toXrandrDisplayName"
    [
    -- Text expands quickly in smallcheck, consuming lots of resources for
    -- little benefit. Reduce smallcheck's depth here to make sure tests run
    -- in a timely manner.
      localOption (SmallCheckDepth 4)
      $ testProperty "should ignore card name and collapse port number"
      $ \(card :: Text, port :: Text, portNumber :: Text) ->
            let displayName = T.intercalate "-" [card, port, portNumber]
            in Display.toXrandrDisplayName displayName == (port <> portNumber)
    ]

buildXrandrCommand = testGroup
    "buildXrandrCommand"
    [ localOption (SmallCheckDepth 3)
      $ testProperty "should construct a valid xrandr command"
      $ \(card :: Text, port :: Text, port' :: Text, portNum :: Text) ->
            let
                displayName p = T.intercalate "-" [card, p, portNum]
                display             = displayName port
                display'            = displayName port'
                activePassiveConfig = Display.ActivePassive
                    (Display.Display display Display.Enabled Display.Connected)
                    (Display.Display display' Display.Disabled Display.Connected
                    )
                xrandrCommand = Display.buildXrandrCommand activePassiveConfig
            in ("--output " <> port <> portNum <> " --off")
                `T.isInfixOf` xrandrCommand
                &&            (port' <> portNum <> " --pos 0x0")
                `T.isInfixOf` xrandrCommand
    ]
