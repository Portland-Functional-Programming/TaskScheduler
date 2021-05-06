module App.TagModal (component) where

import Prelude (($))

import Data.List (List(..))
import Halogen (ClassName(..), mkComponent)
import Halogen as H
import Halogen.HTML as HH
--import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Prop
import App.Model (Tag, Todo)

type Input = Todo
type State = { todo :: Todo
             , tags :: List Tag
             }

component :: forall query output m. H.Component HH.HTML query Input output m
component =
  mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }

initialState :: Input -> State
initialState todo' = { todo: todo'
                    , tags: Nil
                    }

render :: forall action slots m. State -> H.ComponentHTML action slots m
render state = modalCard state tagForm

tagForm :: forall cs action. HH.HTML cs action
tagForm =
  HH.form_ [ HH.div
             [Prop.class_ $ ClassName "field"]
             [ HH.label [Prop.class_ $ ClassName "label"] [HH.text "Name"]
             , HH.div [Prop.class_ $ ClassName "control"]
               [ HH.input [ Prop.class_ $ ClassName "input"
                          , Prop.type_ Prop.InputText
                          , Prop.placeholder "Enter a tag name"]
               ]
             ]
           ]

modalCard :: forall cs action. State -> HH.HTML cs action -> HH.HTML cs action
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
      [HH.button
       [ Prop.classes [ClassName "button", ClassName "is-success"]
         --, HE.onClick \_ -> map (\todo -> SaveTag todo (Tag "home")) state.modalTarget
       ]
       [HH.text "Save Tag"]
      ]
    ]
  ]
