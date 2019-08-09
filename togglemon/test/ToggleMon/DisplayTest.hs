module ToggleMon.DisplayTest where

import           Data.Maybe        (fromMaybe)
import           Test.Tasty        (testGroup)
import           Test.Tasty.HUnit  (assertFailure, testCase, (@?=))
import           TestUtils
import qualified ToggleMon.Display as Display

test_toStatus = testGroup
    "toStatus"
    [ testCase "serialize connected"
    $   Display.toStatus "connected"
    @?= Display.Connected
    , testCase "serialize disconnected"
    $   Display.toStatus "disconnected"
    @?= Display.Disconnected
    -- TODO have a test case that captures exceptions
    ]

test_toEnabled = testGroup
    "toEnabled"
    [ testCase "serialize enabled"
    $   Display.toEnabled "enabled"
    @?= Display.Enabled
    , testCase "serialize disabled"
    $   Display.toEnabled "disabled"
    @?= Display.Disabled
    ]

test_toDisplay = testGroup
    "toDisplay"
    [ testCase "should properly serialize a known monitor" $ do
          let def = Display.Display "" Display.Disabled Display.Disconnected
          let
              expected = Display.Display
                  "card0-eDP-1"
                  Display.Enabled
                  Display.Connected

          result <- runTestMonad $ Display.toDisplay "card0-eDP-1"

          fromMaybe def result @?= expected
    ]
