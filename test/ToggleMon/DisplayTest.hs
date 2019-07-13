module ToggleMon.DisplayTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified ToggleMon.Display as Display

test_toStatus = testGroup "toStatus" [
    testCase "serialize connected" $
      Display.toStatus "connected" @?= Display.Connected,
    testCase "serialize disconnected" $
      Display.toStatus "disconnected" @?= Display.Disconnected
    -- TODO have a test case that captures exceptions
  ]

test_toEnabled = testGroup "toEnabled" [
    testCase "serialize enabled" $
      Display.toEnabled "enabled" @?= Display.Enabled,
    testCase "serialize disabled" $
      Display.toEnabled "disabled" @?= Display.Disabled
  ]
