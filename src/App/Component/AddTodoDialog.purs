module App.Component.AddTodoDialog where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Prop
import Halogen (AttrName(..), ClassName(..))

addTodoDialog :: forall query output m. H.Component query Int output m
addTodoDialog =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval -- { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div
        [ Prop.classes [ClassName "modal", ClassName "is-active"]
        ]
        [ HH.div [Prop.class_ $ ClassName "modal-background" ] []
        , HH.div [Prop.class_ $ ClassName "modal-card"]
          [ HH.header
            [Prop.class_ $ ClassName "modal-card-head"]
            [HH.p [Prop.class_ $ ClassName "modal-card"] [HH.text "Enter a tag"]]
          , HH.section
            [Prop.class_ $ ClassName "modal-card-body"]
            [ HH.form_ [ HH.div
                         [Prop.class_ $ ClassName "field"]
                         [ HH.label [Prop.class_ $ ClassName "label"] [HH.text "Name"]
                         , HH.div [Prop.class_ $ ClassName "control"]
                           [ HH.input [ Prop.value ""
                                      , Prop.class_ $ ClassName "input"
                                      , Prop.type_ Prop.InputText
                                      , Prop.placeholder "Enter a tag name"
                                      --, HE.onValueInput TagTextAdded
                                      ]
                           ]
                         ]
                       ]
            ]
          , HH.footer
            [Prop.class_ $ ClassName "modal-card-foot"]
            [ HH.button [ Prop.classes [ClassName "button", ClassName "is-failure"]
                        -- , HE.onClick (\_ -> CloseTagModal)
                        ]
              [HH.text "Cancel"]
            , HH.button [ Prop.classes [ClassName "button", ClassName "is-success"]
                        -- , HE.onClick (\_ -> let todo = unsafePartial (fromJust state.modalTarget)
                        --                         txt = state.tagText
                        --                     in (SaveTag todo (Tag txt)))
                        ]
              [HH.text "Save Tag"]
            ]
          ]
        ]

  -- handleAction = case _ of
  --   Decrement ->
  --     H.modify_ \state -> state - 1

  --   Increment ->
  --     H.modify_ \state -> state + 1
