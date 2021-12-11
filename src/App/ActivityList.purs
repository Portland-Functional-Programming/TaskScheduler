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
import TaskScheduler.Domain.Task (Task, Priority(..), Tag(..))
import TaskScheduler.Domain.Panel (Panel(..))
import Type.Proxy (Proxy(..))
import App.Component.AddTodoDialog (addTodoDialog)
import App.Component.AddTodoDialog as AddTodoDialog
import Web.Event.Event (Event, preventDefault)
import Web.HTML.Event.DragEvent as DE

allPanels :: Array Panel
allPanels = [ActivityInventoryList, TodoToday, Current]

type TodoNow =
  { todos :: Maybe (Array Task)
  } 

initialTodos :: Array Task
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
    { tasks :: Array Task
    , taskNow :: Maybe TodoNow
    , selectedTask :: Maybe Task
    , transitioning :: Maybe Task
    , modalTarget :: Maybe Task
    , showTagModal :: Boolean
    , showAddTodoModal :: Boolean
    , tagText :: String
    }

data Action = Dragging Task
            | DroppedOn Panel
            | PreventDefault Event Action
            | OpenAddTagModal Task
            | OpenAddTodoModal
            | CloseTagModal
            | SaveTag Task Tag
            | TagTextAdded String
            | HandleAddTodo AddTodoDialog.Output
            | Noop

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { tasks: initialTodos
                          , taskNow: Nothing
                          , selectedTask: Nothing
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
                           [HH.text "Add New Task"]
                         ]
              ]
          ]
      ]
    ]

fromArrayOn :: forall k v. Ord k => (v -> k) -> Array v -> Map k (Array v)
--fromArrayOn :: (Task -> Panel) -> Array Task -> Map Panel (Array Task)
fromArrayOn vk tasks = fromFoldableWith append $ map (\v -> Tuple (vk v) [v]) tasks

panelsView :: forall cs. State -> HH.HTML cs Action
panelsView state =
  HH.div [ Prop.classes [ClassName "column"]]
    [
      HH.section [ Prop.class_ (ClassName "section")]
      [
        HH.div [ Prop.id "Task"]
          [
            HH.div [ Prop.class_ (ClassName "container")]
            (uncurry panelsListView <$> toUnfoldable (allPanels' `unionWith append` panelsWithTodos))
          ]
      ]
    ]
  where allPanels' = fromFoldable (flip Tuple [] <$> allPanels)
        panelsWithTodos = fromArrayOn _.associatedPanel state.tasks

panelsListView :: forall cs. Panel -> Array Task -> HH.HTML cs Action
panelsListView panel tasks =
  HH.div [ Prop.class_ (ClassName "panel"), Prop.id "activityInventoryList"
         , HE.onDragOver (\de -> PreventDefault (DE.toEvent de) Noop)
         , HE.onDrop (\de -> PreventDefault (DE.toEvent de) (DroppedOn panel))
         ]
         [ HH.h1_ [ HH.text (show panel) ]
         , listView tasks
         ]

listView :: forall cs. Array Task -> HH.HTML cs Action
listView tasks =
  HH.div [ Prop.class_ (ClassName "itemContainer")] (map todoView tasks)

tagsView :: forall cs. Array Tag -> HH.HTML cs Action
tagsView tags = HH.ul_ (map (\(Tag tag) -> HH.li_ [HH.text tag]) tags)

todoView :: forall cs. Task -> HH.HTML cs Action
todoView task =
  HH.div
    [ Prop.class_ $ ClassName "item"
    , Prop.attr (AttrName "style")  $ "background-color: " <> priorityToColor task.priority
    , Prop.attr (AttrName "draggable") "true"
    , HE.onDrag (\de -> PreventDefault (DE.toEvent de) (Dragging task))
    ] [ HH.text task.name
      , tagsView task.tags
      , HH.button [ Prop.classes [ClassName "button", ClassName "is-primary"]
                  , HE.onClick (\_ -> OpenAddTagModal task)
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
                        , HE.onClick (\_ -> let task = unsafePartial (fromJust state.modalTarget)
                                                txt = state.tagText
                                            in (SaveTag task (Tag txt)))
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

type Slots = ( addTodoDialog :: forall query . H.Slot query AddTodoDialog.Output Int )

_addTodoDialog = Proxy :: Proxy "addTodoDialog"

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ Prop.class_ (ClassName "columns")]
    [ sidebarView state
    , panelsView state
    , modalView state
    , if state.showAddTodoModal
      then HH.div_ [ HH.slot _addTodoDialog 0 addTodoDialog 0 HandleAddTodo ]
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

splitTodosByName :: Task -> Array Task -> Maybe { init :: Array Task, task :: Task, rest :: Array Task }
splitTodosByName task tasks =
  let { init: init, rest: rest } = span (\t -> t.name == task.name) tasks
      maybeTask = head rest
      rest' = fromMaybe [] (tail rest)
  in (\t -> {init, task: t, rest: rest'}) <$> maybeTask

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Dragging task -> H.modify_ \st -> st { transitioning = Just task }
  DroppedOn panel -> H.modify_ \st ->
    let
      maybeTasks = do
        task <- st.transitioning
        index <- findIndex (\t -> t.name == task.name) st.tasks
        tasks <- modifyAt index (\task' -> task' { associatedPanel = panel }) st.tasks
        Just tasks

      f :: Array Task -> State
      f tasks' = st { tasks = tasks' }
    in maybe st f maybeTasks

  PreventDefault e next -> do
    H.liftEffect $ preventDefault e
    handleAction next

  OpenAddTagModal task -> H.modify_ \st -> st { modalTarget = Just task }
  OpenAddTodoModal -> H.modify_ \st -> st { showAddTodoModal = true }
  CloseTagModal -> H.modify_ \st -> st { modalTarget = Nothing }
  SaveTag task tag -> H.modify_ \st -> let
    maybeTasks = do
      index <- findIndex (\t -> t.name == task.name) st.tasks
      modifyAt index (\task -> task { tags = cons tag task.tags }) st.tasks

    f :: Array Task -> State
    f tasks' = st { tasks = tasks'
                  , modalTarget = Nothing
                  , tagText = ""
                  }
    in maybe st f maybeTasks

  TagTextAdded txt -> H.modify_ \st -> st { tagText = txt }

  HandleAddTodo AddTodoDialog.Canceled ->
    H.modify_ \st -> st { showAddTodoModal = false }

  HandleAddTodo (AddTodoDialog.TaskCreated task) ->
    H.modify_ \st -> st { tasks = cons task st.tasks
                        , showAddTodoModal = false
                        }

  Noop -> pure unit
