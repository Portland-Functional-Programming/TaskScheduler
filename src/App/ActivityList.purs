module App.ActivityList where

import Prelude

import Data.Array (span, tail, head, mapMaybe, singleton)
import Data.Map (fromFoldableWith, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Class (class MonadEffect)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Prop
import Web.Event.Event (Event, preventDefault)
import Web.HTML.Event.DragEvent as DE

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
  , associatedPanel :: Maybe Panel
  }

type TodoNow =
  { todos :: Maybe (Array Todo)
  } 

type ActivityInventoryList =
  { todos :: Maybe (Array Todo)
  }

type Panel =
  { name :: String
  }

initialTodos :: Array Todo
initialTodos = [ { name : "Finish planning"
                 , priority : High
                 , tags: [Tag "work", Tag "home"]
                 , associatedPanel : head initialPanels
                 }
               , { name : "next"
                 , priority : Medium
                 , tags : [Tag "work"]
                 , associatedPanel : head initialPanels
                 }
               , { name : "trivial task"
                 , priority : Medium
                 , tags : [Tag "home"]
                 , associatedPanel : head initialPanels
                 }
               ]

initialPanels :: Array Panel
initialPanels = [ { name: "Activity Inventory List"
                  }
                , { name: "Todo Today"
                  }
                , { name: "Currently doing"
                  }
                ]

type State =
    { panels :: Array Panel
    , todos :: Array Todo
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

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { panels: initialPanels
                          , todos: initialTodos
                          , todoNow: Nothing
                          , selectedTodo: Nothing
                          , transitioning: Nothing
                          , showTagModal: false
                          , modalTarget: Nothing
                          }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

sidebarView :: forall cs m. State -> HH.HTML cs m
sidebarView state =
  HH.aside [ Prop.classes [ClassName "column", ClassName "sidebar", ClassName "is-narrow"]]
    [
      HH.section [ Prop.class_ (ClassName "section")]
      [
        HH.nav [ Prop.class_ (ClassName "menu")]
          [
            HH.ul [ Prop.class_ (ClassName "menu-list")]
              [
                HH.li [] [ HH.text "test"]
              ]
          ]
      ]
    ]

fromFoldableOn :: forall k v. Ord k => (v -> Maybe k) -> Array v -> Array (Tuple k (Array v))
fromFoldableOn vk todos = toUnfoldable $ fromFoldableWith append $ mapMaybe (\v -> flip Tuple [v] <$> vk v) todos

panelsView :: forall cs. State -> HH.HTML cs Action
panelsView state =
  HH.div [ Prop.classes [ClassName "column"]]
    [
      HH.section [ Prop.class_ (ClassName "section")]
      [
        HH.div [ Prop.id_"Todo"]
          [
            HH.div [ Prop.class_ (ClassName "container")]
            (uncurry panelsListView <$> fromFoldableOn (\t -> t.associatedPanel) state.todos)
          ]
      ]
    ]

panelsListView :: forall cs. Panel -> Array Todo -> HH.HTML cs Action
panelsListView panel todos =
  HH.div [ Prop.class_ (ClassName "panel"), Prop.id_ "activityInventoryList"
         , HE.onDragOver (\de -> PreventDefault (DE.toEvent de) Noop)
         , HE.onDrop (\de -> PreventDefault (DE.toEvent de) (DroppedOn panel))
         ]
         [ HH.h1_ [ HH.text panel.name ]
         , listView todos
         ]


listView :: forall cs. Array Todo -> HH.HTML cs Action
listView todos =
  HH.div [ Prop.class_ (ClassName "itemContainer")] (map todoView todos)

tagsView :: forall cs. Array Tag -> HH.HTML cs Action
tagsView tags = HH.ul_ (map (\(Tag tag) -> HH.li_ [HH.text tag]) tags)

todoView :: forall cs. Todo -> HH.HTML cs Action
todoView todo =
  HH.div
    [ Prop.class_ $ ClassName "item"
    , Prop.attr (AttrName "style")  $ "background-color: " <> priorityToColor todo.priority
    , Prop.attr (AttrName "draggable") "true"
    ] [ HH.text  todo.name
      , HH.button [ Prop.classes [ClassName "button", ClassName "is-primary"]
                  ]
                  [ HH.text "Add Tag"]
      ]

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
                       ]
                       [HH.text "Save Tag"]
            ]
          ]
        ]

modalView :: forall cs. State -> HH.HTML cs Action
modalView state = modalCard state tagForm

-- Helper
priorityToColor :: Priority -> String
priorityToColor priority =
  case priority of
    High -> "#f9aeae"
    Medium -> "#f5f588"
    Low -> "#469dd0"

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state =
  HH.div [ Prop.class_ (ClassName "columns")]
    [ sidebarView state
    , panelsView state
    , modalView state
    ]

splitTodosByName :: Todo -> Array Todo -> Maybe { init :: Array Todo, todo :: Todo, rest :: Array Todo }
splitTodosByName todo todos =
  let { init: init, rest: rest } = span (\t -> t.name == todo.name) todos
      maybeTodo = head rest
      rest' = fromMaybe [] (tail rest)
  in map (\todo -> {init, todo, rest: rest'}) maybeTodo

handleAction :: forall cs o m. MonadEffect m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Dragging todo -> H.modify_ \st -> st { transitioning = Just todo }
  DroppedOn panel -> H.modify_ \st ->
    let maybeTodos = do
          todo <- st.transitioning
          {init, todo: draggedTodo, rest} <- splitTodosByName todo st.todos
          let draggedTodo' = draggedTodo { associatedPanel = Just panel }
              todos' = init <> singleton draggedTodo' <> rest
          Just todos'
    in maybe st
             (\todos -> st { transitioning = Nothing
                            , todos = todos
                            }
             )
             maybeTodos
  PreventDefault e next -> do
    H.liftEffect $ preventDefault e
    handleAction next

  OpenAddTagModal todo -> H.modify_ \st -> st { modalTarget = Just todo }
  SaveTag _ _ -> H.modify_ \st -> st { modalTarget = Nothing }
  Noop -> pure unit
