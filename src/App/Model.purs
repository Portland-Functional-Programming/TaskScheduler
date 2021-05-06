module App.Model where

import Prelude

data Priority = High
              | Medium
              | Low

derive instance eqPriority :: Eq Priority

data Tag = Tag String
derive instance eqTag :: Eq Tag

type Todo =
  { name :: String
  , priority :: Priority
  , tags :: Array Tag
  }
