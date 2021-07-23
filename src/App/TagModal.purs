module App.TagModal (component, Output(..)) where

import Prelude (($), Unit)

import Data.List (List(..), (:))
import Data.Maybe (Maybe(Just))
import Halogen (ClassName(..), mkComponent)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Prop
import App.Model (Tag(..), Todo)

type Input = Todo
data Output
  =
   --  TagsAdded Todo (List Tag)
  -- |
    TagModalCanceled
type State = { todo :: Todo
             , tags :: List Tag
             }
data Action
  = AddTag Tag
  | Cancel
-- data Query a
--   = SetElement (Maybe HH.HTMLElement) a

component :: forall query m. H.Component HH.HTML query Input Output m
component =
  mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState todo' = { todo: todo'
                    , tags: Nil
                    }

handleAction :: forall m. MonadEffect => Action -> H.HalogenM State Action () Output m Unit
handleAction (AddTag tag) = do
  response <- Ajax.post 
handleAction Cancel = H.raise TagModalCanceled  -- Tell the parent component to close the tag modal.

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state = modalCard state tagForm

tagForm :: forall cs action. HH.HTML cs action
tagForm =
  HH.form_ [ HH.div
             [Prop.class_ $ ClassName "field"]
             [ HH.label [Prop.class_ $ ClassName "label"] [HH.text "Name"]
             , HH.div [Prop.class_ $ ClassName "control"]
               [ HH.input [ Prop.class_ $ ClassName "input"
                          , Prop.type_ Prop.InputText
                          , Prop.placeholder "Enter a tag name"
                          --, Prop.ref
                          ]
               ]
             ]
           ]

modalCard :: forall cs. State -> HH.HTML cs Action -> HH.HTML cs Action
modalCard state content =
  HH.div
  [ Prop.classes [ClassName "modal", ClassName "is-active"]]
  [ HH.div [Prop.class_ $ ClassName "modal-background" ] []
  , HH.div
    [Prop.class_ $ ClassName "modal-card"]
    [ HH.header
      [Prop.class_ $ ClassName "modal-card-head"]
      [HH.p [Prop.class_ $ ClassName "modal-card"] [HH.text "Enter a tag"]]
    , HH.section
      [Prop.class_ $ ClassName "modal-card-body"]
      [content]
    , HH.footer
      [Prop.class_ $ ClassName "modal-card-foot"]
      [ HH.button
        [ Prop.classes [ClassName "button", ClassName "is-primary"]
        , HE.onClick \_ -> Just $ AddTag (Tag "home")
        ]
        [HH.text "Save Tag"]
      , HH.button
        [ Prop.class_ $ ClassName "button"
        , HE.onClick \_ -> Just Cancel
        ]
        [HH.text "Cancel"]
      ]
    ]
  ]
