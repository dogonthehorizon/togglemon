module ToggleMon.IOTest where

import           Control.Exception (Exception, throw)
import qualified Data.Text         as T
import           Test.Tasty        (testGroup)
import           Test.Tasty.HUnit  (assertFailure, testCase, (@?=))
import           TestUtils
import qualified ToggleMon.IO      as ToggleIO
import           ToggleMon.Monad

data SimpleException = SimpleException deriving Show
instance Exception SimpleException

simpleDirContents :: [String]
simpleDirContents = show <$> [1 .. 3]

simpleListDir :: ListDirectoryAction
simpleListDir "happy" = return simpleDirContents
simpleListDir _       = throw SimpleException

test_module = testGroup
    "ToggleMon.IO"
    [ ToggleMon.IOTest.listDirectory
    , filterDirectory
    , ToggleMon.IOTest.readFile
    , exec
    ]

listDirectory = testGroup
    "listDirectory"
    [ testCase "should return the contents of the given directory" $ do
        result <- runTestMonad'
            (mockEnv { envListDirFn = simpleListDir })
            (ToggleIO.listDirectory "happy")
        result @?= Just (T.pack <$> simpleDirContents)
    , testCase "should handle IO exceptions" $ do
        result <- runTestMonad'
            (mockEnv { envListDirFn = simpleListDir })
            (ToggleIO.listDirectory "sad")
        result @?= Nothing
    ]

filterDirectory = testGroup
    "filterDirectory"
    [ testCase "should only return directories" $ assertFailure "TODO"
    , testCase "should handle IO exceptions" $ assertFailure "TODO"
    ]

readFile = testGroup
    "readFile"
    [ testCase "should return file contents" $ assertFailure "TODO"
    , testCase "should remove leading/trailing whitespace"
        $ assertFailure "TODO"
    , testCase "should handle IO exceptions" $ assertFailure "TODO"
    ]

exec = testGroup
    "exec"
    [ testCase "should execute the given  command" $ assertFailure "TODO"
    , testCase "should handle missing command arguments" $ assertFailure "TODO"
    , testCase "should handle IO exceptions" $ assertFailure "TODO"
    ]
