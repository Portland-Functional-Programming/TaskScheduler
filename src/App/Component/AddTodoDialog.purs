module App.Component.AddTodoDialog where

import Prelude (bind, map, not, pure, show, unit, ($), (<<<), (>>>))

import Control.Monad.State.Class (get)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (null, toLower, split, trim, Pattern(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Prop
import Halogen (ClassName(..))
import Partial.Unsafe (unsafePartial) -- Shame!
import TaskScheduler.Domain.Task (Task, Priority(..), Tag(..))
import TaskScheduler.Domain.Panel (Panel(ActivityInventoryList))

data Output = Canceled
            | TaskCreated Task

data Action = TaskTitleAdded String
            | PrioritySelected Priority
            | TagsEntered String
            | CancelClicked
            | SaveClicked

type State = { title :: String
             , priority :: Priority
             , tags :: Array Tag
             }

defaultPriority :: Priority
defaultPriority = Low

addTodoDialog :: forall query m. H.Component query Int Output m
addTodoDialog =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = { title: "sample"
                   , priority: defaultPriority
                   , tags: []
                   }

  render _ =
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
            [ HH.form_
              [ -- Task title
                HH.div
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

                -- Task priority
              , HH.div
                [Prop.class_ $ ClassName "field"]
                [ HH.label [Prop.class_ $ ClassName "label"] [HH.text "Priority"]
                , HH.div [Prop.class_ $ ClassName "control"]
                  [ HH.select
                    [ HE.onValueChange (\s -> PrioritySelected $ unsafePartial $ fromJust $ priorityFromString s)]
                    let mkOption :: forall w i. Priority -> HH.HTML w i
                        mkOption p = HH.option [Prop.value <<< toLower <<< show $ p ] [HH.text $ show p]
                    in [mkOption Low, mkOption Medium, mkOption High]
                  ]
                ]

                -- Tags
              , HH.div
                [Prop.class_ $ ClassName "field"]
                [ HH.label [Prop.class_ $ ClassName "label"] [HH.text "Tags"]
                , HH.div [Prop.class_ $ ClassName "control"]
                  [ HH.input [ Prop.value ""
                             , Prop.class_ $ ClassName "input"
                             , Prop.type_ Prop.InputText
                             , Prop.placeholder "Enter tags as comma-separated values"
                             , HE.onValueInput TagsEntered
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
    TaskTitleAdded title -> H.modify_ \st -> st { title = title }
    PrioritySelected priority -> H.modify_ \st -> st { priority = priority }
    TagsEntered tagsString ->
      let tags = split (Pattern ",") >>> map (trim >>> Tag) $ tagsString
      in H.modify_ \st -> st { tags = tags }
    CancelClicked -> H.raise Canceled
    SaveClicked -> do 
      taskData <- get
      if (not <<< null $ taskData.title)
        then H.raise $ TaskCreated { title: taskData.title
                                   , priority: taskData.priority
                                   , tags: taskData.tags
                                   , associatedPanel: ActivityInventoryList
                                   }
        else pure unit

priorityFromString :: String -> Maybe Priority
priorityFromString s = case toLower s of
  "low" -> Just Low
  "medium" -> Just Medium
  "high" -> Just High
  _ -> Nothing
