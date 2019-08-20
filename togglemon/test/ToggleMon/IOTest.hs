module ToggleMon.IOTest where

import           Control.Exception (Exception, throw)
import qualified Data.Text         as T
import           Test.Tasty        (testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))
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

test_listDirectory = testGroup
    "listDirectory"
    [ testCase
            "should return the contents of the given directory using the provided env function"
        $ do
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
