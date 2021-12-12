module TaskScheduler.Domain.Task where

import Prelude

import TaskScheduler.Domain.Panel (Panel)

type Task =
  { title :: String
  , priority :: Priority
  , tags :: Array Tag
  , associatedPanel :: Panel
  }

data Priority = High
              | Medium
              | Low

derive instance eqPriority :: Eq Priority

data Tag = Tag String
derive instance eqTag :: Eq Tag
