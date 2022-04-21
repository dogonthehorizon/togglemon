module MemorableNameTest where

import Control.Monad (replicateM_)
import MemorableName
import Test.SmallCheck (monadic)
import Test.SmallCheck.Series.MemorableName ()
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.SmallCheck (testProperty)

test_show =
  testProperty "MemorableName should stringify as 'word-word'" $
    \(name :: MemorableName) -> case name of
      (MemorableName adj n) -> (adj <> "-" <> n) == show name

test_randInt =
  testProperty "getRandUpTo should return a number between 0 and n-1" $
    \(n :: Int) -> monadic $ do
      rando <- getRandUpTo n :: IO Int
      return $ (rando == 0 && n <= 0) || ((rando >= 0) && (rando < n))

test_memorableName =
  testCase "memorableName should return a memorable name" $
    replicateM_ 100 $
      do
        (MemorableName adj n) <- memorableName
        assertBool
          "Generated a memorable name that wasn't in the default list of nouns or adjectives"
          $ (adj `elem` adjectives)
            && (n `elem` nouns)
