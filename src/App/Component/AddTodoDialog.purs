module App.Component.AddTodoDialog where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

addTodoDialog =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval -- { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [] [ HH.text "-" ]
      , HH.text (show state)
      , HH.button [] [ HH.text "+" ]
      ]

  -- handleAction = case _ of
  --   Decrement ->
  --     H.modify_ \state -> state - 1

  --   Increment ->
  --     H.modify_ \state -> state + 1
