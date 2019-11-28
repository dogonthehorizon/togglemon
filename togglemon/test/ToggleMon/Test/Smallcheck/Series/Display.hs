-- TODO module header, orphan instances for togglemon types
module ToggleMon.Test.Smallcheck.Series.Display where

import Data.Edid                        (Edid, EdidVersion, Manufacturer)
import Test.SmallCheck.Series
import Test.SmallCheck.Series.Instances ()
import ToggleMon.Display

instance Monad m => Serial m Status where
instance Monad m => Serial m Enabled where

instance Monad m => Serial m EdidVersion where
instance Monad m => Serial m Manufacturer where
instance Monad m => Serial m Edid where

instance Monad m => Serial m Display where
