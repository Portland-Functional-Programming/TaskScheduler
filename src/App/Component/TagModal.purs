module App.Component.TagModal
       ( component
       , Slot
       , Output(..)
       ) where

import Prelude (($), (<>), (>>>), Unit, map, unit, pure, bind)

import Data.String (split, trim, Pattern(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Prop
import Halogen (ClassName(..))
import TaskScheduler.Domain.Task (Task, Tag(..))

data Output = Canceled
            | TagsAdded Task (Array Tag)

type State = { task :: Task
             , tags :: Array Tag
             }

data Action = CancelClicked
            | SaveClicked
            | TagTextAdded String

type Slot query id = H.Slot query Output id

component :: forall query m. H.Component query Task Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
    initialState task = { task: task
                        , tags: []
                        }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state =
  HH.div
        [ Prop.classes [ClassName "modal", ClassName "is-active"]
        ]
        [ HH.div [Prop.class_ $ ClassName "modal-background" ] []
        , HH.div [Prop.class_ $ ClassName "modal-card"]
          [ HH.header
            [Prop.class_ $ ClassName "modal-card-head"]
            [HH.p [Prop.class_ $ ClassName "modal-card"] [HH.text $ "Add tags to task \"" <> state.task.title <> "\"" ]]
          , HH.section
            [Prop.class_ $ ClassName "modal-card-body"]
            [ HH.form_
              [ HH.div
                [Prop.class_ $ ClassName "field"]
                [ HH.label [Prop.class_ $ ClassName "label"] [HH.text "Name"]
                , HH.div [Prop.class_ $ ClassName "control"]
                  [ HH.input [ Prop.value ""
                             , Prop.class_ $ ClassName "input"
                             , Prop.type_ Prop.InputText
                             , Prop.placeholder "Enter a tag name"
                             , HE.onValueInput TagTextAdded]
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
              [HH.text "Save"]
            ]
          ]
        ]

handleAction :: forall slots m. Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  CancelClicked -> H.raise Canceled
  SaveClicked -> do
    st <- H.get
    H.raise $ TagsAdded st.task st.tags
  TagTextAdded s ->
    -- Copied from AddTodoDialog
    let tags = split (Pattern ",") >>> map (trim >>> Tag) $ s
    in H.modify_ \st -> st { tags = tags }
