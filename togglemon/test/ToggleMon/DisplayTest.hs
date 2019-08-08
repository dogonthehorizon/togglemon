module ToggleMon.DisplayTest where

import Data.Maybe (fromMaybe)
import           Test.Tasty        (testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=), assertFailure)
import qualified ToggleMon.Display as Display
import TestUtils

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

test_toDisplay = testGroup "toDisplay" [
    testCase "happy path" $
      -- TODO in theory this is what we want to do. Currently failing to compile.
      --      Perhaps there's something in tasty that let's us run monadic things
      runTestMonad $ do
        mDisplay <- Display.toDisplay "card0-eDP-1"
        (fromMaybe (assertFailure "boo boo") mDisplay) @?= (Display.Display "card0-eDP-1" Display.Enabled Display.Connected)
  ]
