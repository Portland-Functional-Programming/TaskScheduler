module TaskScheduler.Domain.Panel where

import Prelude

data Panel = ActivityInventoryList
           | TodoToday
           | Current

instance showPanel :: Show Panel where
  show ActivityInventoryList = "Activity Inventory List"
  show TodoToday = "Todo Today"
  show Current = "Current"

derive instance eqPanel :: Eq Panel
derive instance ordPanel :: Ord Panel
