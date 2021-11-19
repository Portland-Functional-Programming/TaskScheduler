module App.ActivityList where

import Prelude

import Data.Array (head, mapMaybe, singleton, span, tail, findIndex, modifyAt, cons)
import Data.Map (Map, fromFoldable, fromFoldableWith, toUnfoldable, unionWith)
import Data.Maybe (Maybe(..), fromMaybe, fromJust, maybe, isJust)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Class (class MonadEffect)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Prop
import Partial.Unsafe (unsafePartial) -- Shame!
import Type.Proxy (Proxy(..))
import App.Component.AddTodoDialog (addTodoDialog)
import Web.Event.Event (Event, preventDefault)
import Web.HTML.Event.DragEvent as DE

data Priority = High
              | Medium
              | Low

derive instance eqPriority :: Eq Priority

data Tag = Tag String
derive instance eqTag :: Eq Tag

data Panel = ActivityInventoryList
           | TodoToday
           | Current

instance showPanel :: Show Panel where
  show ActivityInventoryList = "Activity Inventory List"
  show TodoToday = "Todo Today"
  show Current = "Current"

derive instance eqPanel :: Eq Panel
derive instance ordPanel :: Ord Panel

allPanels :: Array Panel
allPanels = [ActivityInventoryList, TodoToday, Current]

type Todo =
  { name :: String
  , priority :: Priority
  , tags :: Array Tag
  , associatedPanel :: Panel
  }

type TodoNow =
  { todos :: Maybe (Array Todo)
  } 

initialTodos :: Array Todo
initialTodos = [ { name : "Finish planning"
                 , priority : High
                 , tags: [Tag "work", Tag "home"]
                 , associatedPanel : ActivityInventoryList
                 }
               , { name : "next"
                 , priority : Medium
                 , tags : [Tag "work"]
                 , associatedPanel : TodoToday
                 }
               , { name : "trivial task"
                 , priority : Medium
                 , tags : [Tag "home"]
                 , associatedPanel : TodoToday
                 }
               ]

type State =
    { todos :: Array Todo
    , todoNow :: Maybe TodoNow
    , selectedTodo :: Maybe Todo
    , transitioning :: Maybe Todo
    , modalTarget :: Maybe Todo
    , showTagModal :: Boolean
    , showAddTodoModal :: Boolean
    , tagText :: String
    }

data Action = Dragging Todo
            | DroppedOn Panel
            | PreventDefault Event Action
            | OpenAddTagModal Todo
            | OpenAddTodoModal
            | CloseTagModal
            | SaveTag Todo Tag
            | TagTextAdded String
            | Noop

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { todos: initialTodos
                          , todoNow: Nothing
                          , selectedTodo: Nothing
                          , transitioning: Nothing
                          , showTagModal: false
                          , showAddTodoModal: false
                          , modalTarget: Nothing
                          , tagText: ""
                          }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

sidebarView :: forall cs. State -> HH.HTML cs Action
sidebarView state =
  HH.aside [ Prop.classes [ClassName "column", ClassName "sidebar", ClassName "is-narrow"]]
    [
      HH.section [ Prop.class_ (ClassName "section")]
      [
        HH.nav [ Prop.class_ (ClassName "menu")]
          [
            HH.ul [ Prop.class_ (ClassName "menu-list")]
              [
                HH.li [] [ HH.button
                           [ Prop.classes [ClassName "button", ClassName "is-primary"]
                           , HE.onClick (\_ -> OpenAddTodoModal)
                           ]
                           [HH.text "Add New Todo"]
                         ]
              ]
          ]
      ]
    ]

fromArrayOn :: forall k v. Ord k => (v -> k) -> Array v -> Map k (Array v)
--fromArrayOn :: (Todo -> Panel) -> Array Todo -> Map Panel (Array Todo)
fromArrayOn vk todos = fromFoldableWith append $ map (\v -> Tuple (vk v) [v]) todos

panelsView :: forall cs. State -> HH.HTML cs Action
panelsView state =
  HH.div [ Prop.classes [ClassName "column"]]
    [
      HH.section [ Prop.class_ (ClassName "section")]
      [
        HH.div [ Prop.id "Todo"]
          [
            HH.div [ Prop.class_ (ClassName "container")]
            (uncurry panelsListView <$> toUnfoldable (allPanels' `unionWith append` panelsWithTodos))
          ]
      ]
    ]
  where allPanels' = fromFoldable (flip Tuple [] <$> allPanels)
        panelsWithTodos = fromArrayOn _.associatedPanel state.todos

panelsListView :: forall cs. Panel -> Array Todo -> HH.HTML cs Action
panelsListView panel todos =
  HH.div [ Prop.class_ (ClassName "panel"), Prop.id "activityInventoryList"
         , HE.onDragOver (\de -> PreventDefault (DE.toEvent de) Noop)
         , HE.onDrop (\de -> PreventDefault (DE.toEvent de) (DroppedOn panel))
         ]
         [ HH.h1_ [ HH.text (show panel) ]
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
    , HE.onDrag (\de -> PreventDefault (DE.toEvent de) (Dragging todo))
    ] [ HH.text todo.name
      , tagsView todo.tags
      , HH.button [ Prop.classes [ClassName "button", ClassName "is-primary"]
                  , HE.onClick (\_ -> OpenAddTagModal todo)
                  ]
                  [ HH.text "Add Tag"]
      ]

tagForm :: forall cs. State -> HH.HTML cs Action
tagForm state =
  HH.form_ [ HH.div
             [Prop.class_ $ ClassName "field"]
             [ HH.label [Prop.class_ $ ClassName "label"] [HH.text "Name"]
             , HH.div [Prop.class_ $ ClassName "control"]
               [ HH.input [ Prop.value state.tagText
                          , Prop.class_ $ ClassName "input"
                          , Prop.type_ Prop.InputText
                          , Prop.placeholder "Enter a tag name"
                          , HE.onValueInput TagTextAdded]
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
            [ HH.button [ Prop.classes [ClassName "button", ClassName "is-failure"]
                        , HE.onClick (\_ -> CloseTagModal)]
              [HH.text "Cancel"]
            , HH.button [ Prop.classes [ClassName "button", ClassName "is-success"]
                        , HE.onClick (\_ -> let todo = unsafePartial (fromJust state.modalTarget)
                                                txt = state.tagText
                                            in (SaveTag todo (Tag txt)))
                        ]
              [HH.text "Save Tag"]
            ]
          ]
        ]

modalView :: forall cs. State -> HH.HTML cs Action
modalView state = modalCard state (tagForm state)

-- Helper
priorityToColor :: Priority -> String
priorityToColor priority =
  case priority of
    High -> "#f9aeae"
    Medium -> "#f5f588"
    Low -> "#469dd0"

type Slots = ( addTodoDialog :: forall query . H.Slot query Void Int )

_addTodoDialog = Proxy :: Proxy "addTodoDialog"

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ Prop.class_ (ClassName "columns")]
    [ sidebarView state
    , panelsView state
    , modalView state
    , if state.showAddTodoModal
      then HH.div_ [ HH.slot_ _addTodoDialog 0 addTodoDialog 0]
      else HH.div_ []
    ]

-- slot_
--   :: forall query action input output slots m label slot _1
--    . Row.Cons label (Slot query output slot) _1 slots
--   => IsSymbol label
--   => Ord slot
--   => Proxy label
--   -> slot
--   -> Component query input output m
--   -> input
--   -> ComponentHTML action slots m  

splitTodosByName :: Todo -> Array Todo -> Maybe { init :: Array Todo, todo :: Todo, rest :: Array Todo }
splitTodosByName todo todos =
  let { init: init, rest: rest } = span (\t -> t.name == todo.name) todos
      maybeTodo = head rest
      rest' = fromMaybe [] (tail rest)
  in (\t -> {init, todo: t, rest: rest'}) <$> maybeTodo

handleAction :: forall cs o m. MonadEffect m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Dragging todo -> H.modify_ \st -> st { transitioning = Just todo }
  DroppedOn panel -> H.modify_ \st ->
    let
      maybeTodos = do
        todo <- st.transitioning
        index <- findIndex (\t -> t.name == todo.name) st.todos
        todos <- modifyAt index (\todo' -> todo' { associatedPanel = panel }) st.todos
        Just todos

      f :: Array Todo -> State
      f todos' = st { todos = todos' }
    in maybe st f maybeTodos

  PreventDefault e next -> do
    H.liftEffect $ preventDefault e
    handleAction next

  OpenAddTagModal todo -> H.modify_ \st -> st { modalTarget = Just todo }
  OpenAddTodoModal -> H.modify_ \st -> st { showAddTodoModal = true }
  CloseTagModal -> H.modify_ \st -> st { modalTarget = Nothing }
  SaveTag todo tag -> H.modify_ \st -> let
    maybeTodos = do
      index <- findIndex (\t -> t.name == todo.name) st.todos
      modifyAt index (\todo -> todo { tags = cons tag todo.tags }) st.todos

    f :: Array Todo -> State
    f todos' = st { todos = todos'
                  , modalTarget = Nothing
                  , tagText = ""
                  }
    in maybe st f maybeTodos

  TagTextAdded txt -> H.modify_ \st -> st { tagText = txt }

  Noop -> pure unit
