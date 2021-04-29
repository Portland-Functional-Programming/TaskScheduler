module App.TagModal where

import Prelude

import Data.Maybe (isJust)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Prop
import App.Model

tagForm :: forall cs. HH.HTML cs Action
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

modalCard :: forall cs. State -> HH.HTML cs Action -> HH.HTML cs Action
modalCard state content =
  HH.div
        [ Prop.classes let classes = if isJust state.modalTarget
                                     then [ClassName "modal", ClassName "is-active"]
                                     else [ClassName "modal"]
                       in classes
        ]
        [ HH.div [Prop.class_ $ ClassName "modal-background" ] []
        , HH.div [Prop.class_ $ ClassName "modal-card"]
          [ HH.header
            [Prop.class_ $ ClassName "modal-card-head"]
            [HH.p [Prop.class_ $ ClassName "modal-card"] [HH.text "Enter a tag"]]
          , HH.section
            [Prop.class_ $ ClassName "modal-card-body"]
            [content]
          , HH.footer
            [Prop.class_ $ ClassName "modal-card-foot"]
            [HH.button [ Prop.classes [ClassName "button", ClassName "is-success"]
                       , HE.onClick \_ -> map (\todo -> SaveTag todo (Tag "home")) state.modalTarget]
                       [HH.text "Save Tag"]
            ]
          ]
        ]

modalView :: forall cs. State -> HH.HTML cs Action
modalView state = modalCard state tagForm
