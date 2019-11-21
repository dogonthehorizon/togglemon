module Test.SmallCheck.Series.MemorableName where

import MemorableName
import Test.SmallCheck.Series
import Test.SmallCheck.Series.Instances ()

instance Monad m => Serial m MemorableName where
  series = cons2 MemorableName
