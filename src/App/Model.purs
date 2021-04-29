module App.Model where

import Prelude

import Data.Maybe (Maybe)
import Web.Event.Event (Event)

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

type TodoNow =
  { todos :: Maybe (Array Todo)
  }

type ActivityInventoryList =
  { todos :: Maybe (Array Todo)
  }

type Panel =
  { name :: String
  , todos :: Array Todo
  }

type State =
    { panels :: Array Panel
    , todoNow :: Maybe TodoNow
    , selectedTodo :: Maybe Todo
    , transitioning :: Maybe Todo
    , modalTarget :: Maybe Todo
    , showTagModal :: Boolean
    }

data Action = Dragging Todo
            | DroppedOn Panel
            | PreventDefault Event Action
            | OpenAddTagModal Todo
            | SaveTag Todo Tag
            | Noop
