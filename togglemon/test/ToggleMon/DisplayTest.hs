module ToggleMon.DisplayTest where

import           Data.Maybe                               (fromMaybe)
import           Test.Tasty                               (testGroup)
import           Test.Tasty.HUnit                         (testCase, (@?=))
import           Test.Tasty.SmallCheck                    (testProperty)
import           TestUtils
import qualified ToggleMon.Display                        as Display
import           ToggleMon.Test.Smallcheck.Series.Display ()

test_toStatus = testGroup
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

test_toEnabled = testGroup
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

test_toDisplay = testGroup
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

test_getActiveDisplay = testGroup
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

test_getDisabledDisplay = testGroup
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
