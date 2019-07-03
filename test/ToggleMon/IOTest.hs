module ToggleMon.IOTest where

import ToggleMon.IO
import           Test.Tasty                       (TestTree)
import           Test.Tasty.HUnit                 (testCase)

test_foo :: TestTree
test_foo = testCase "foo" $
  return ()
