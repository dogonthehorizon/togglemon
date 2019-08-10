-- TODO module header, orphan instances for togglemon types
module ToggleMon.Test.Smallcheck.Series.Display where

import Test.SmallCheck.Series
import Test.SmallCheck.Series.Instances ()
import ToggleMon.Display

instance Monad m => Serial m Status where
  series = cons0 Connected \/ cons0 Disconnected

instance Monad m => Serial m Enabled where
  series = cons0 Enabled \/ cons0 Disabled

instance Monad m => Serial m Display where
  series = cons3 Display
