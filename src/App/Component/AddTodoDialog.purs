module App.Component.AddTodoDialog where

import Prelude

import Control.Monad.State.Class (get)
import Data.String (null)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Prop
import Halogen (AttrName(..), ClassName(..))
import TaskScheduler.Domain.Task (Task, Priority(Medium))
import TaskScheduler.Domain.Panel (Panel(ActivityInventoryList))

data Output = Canceled
            | TaskCreated Task

data Action = TaskTitleAdded String
            | CancelClicked
            | SaveClicked

data State = String

addTodoDialog :: forall query output m. H.Component query Int Output m
addTodoDialog =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = ""

  render state =
    HH.div
        [ Prop.classes [ClassName "modal", ClassName "is-active"]
        ]
        [ HH.div [Prop.class_ $ ClassName "modal-background" ] []
        , HH.div [Prop.class_ $ ClassName "modal-card"]
          [ HH.header
            [Prop.class_ $ ClassName "modal-card-head"]
            [HH.p [Prop.class_ $ ClassName "modal-card"] [HH.text "Create A Task"]]
          , HH.section
            [Prop.class_ $ ClassName "modal-card-body"]
            [ HH.form_ [ HH.div
                         [Prop.class_ $ ClassName "field"]
                         [ HH.label [Prop.class_ $ ClassName "label"] [HH.text "Title"]
                         , HH.div [Prop.class_ $ ClassName "control"]
                           [ HH.input [ Prop.value ""
                                      , Prop.class_ $ ClassName "input"
                                      , Prop.type_ Prop.InputText
                                      , Prop.placeholder "Enter task title"
                                      , HE.onValueInput TaskTitleAdded
                                      ]
                           ]
                         ]
                       ]
            ]
          , HH.footer
            [Prop.class_ $ ClassName "modal-card-foot"]
            [ HH.button
              [ Prop.classes [ClassName "button", ClassName "is-failure"]
              , HE.onClick (\_ -> CancelClicked)
              ]
              [HH.text "Cancel"]
            , HH.button [ Prop.classes [ClassName "button", ClassName "is-success"]
                        , HE.onClick (\_ -> SaveClicked)
                        ]
              [HH.text "Save Task"]
            ]
          ]
        ]

  handleAction = case _ of
    TaskTitleAdded title -> H.put title
    CancelClicked -> H.raise Canceled
    SaveClicked -> do
      title <- get
      if (not <<< null $ title)
        then H.raise $ TaskCreated { title: title
                                   , priority: Medium
                                   , tags: []
                                   , associatedPanel: ActivityInventoryList
                                   }
        else pure unit
