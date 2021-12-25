module App.ActivityList where

import Prelude

import Data.Array (head, span, tail, findIndex, modifyAt, cons)
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
import App.Component.AddTodoDialog as AddTodoDialog
import App.Component.TagModal as TagModal
import Web.Event.Event (Event, preventDefault)
import Web.HTML.Event.DragEvent as DE

allPanels :: Array Panel
allPanels = [ActivityInventoryList, TodoToday, Current]

type TodoNow =
  { todos :: Maybe (Array Task)
  } 

initialTodos :: Array Task
initialTodos = [ { title : "Finish planning"
                 , priority : High
                 , tags: [Tag "work", Tag "home"]
                 , associatedPanel : ActivityInventoryList
                 }
               , { title : "next"
                 , priority : Medium
                 , tags : [Tag "work"]
                 , associatedPanel : TodoToday
                 }
               , { title : "trivial task"
                 , priority : Medium
                 , tags : [Tag "home"]
                 , associatedPanel : TodoToday
                 }
               ]

type State =
    { tasks :: Array Task
    , transitioning :: Maybe Task
    , modalTarget :: Maybe Task
    , showAddTodoModal :: Boolean
    }

data Action = Dragging Task
            | DroppedOn Panel
            | PreventDefault Event Action
            | OpenAddTagModal Task
            | OpenAddTodoModal
            | CloseTagModal
            | HandleAddTodo AddTodoDialog.Output
            | HandleTags TagModal.Output
            | Noop

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { tasks: initialTodos
                          , transitioning: Nothing
                          , showAddTodoModal: false
                          , modalTarget: Nothing
                          }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

sidebarView :: forall cs. State -> HH.HTML cs Action
sidebarView _ =
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
    ] [ HH.text task.title
      , tagsView task.tags
      , HH.button [ Prop.classes [ClassName "button", ClassName "is-primary"]
                  , HE.onClick (\_ -> OpenAddTagModal task)
                  ]
                  [ HH.text "Add Tag"]
      ]

-- Helper
priorityToColor :: Priority -> String
priorityToColor priority =
  case priority of
    High -> "#f9aeae"
    Medium -> "#f5f588"
    Low -> "#469dd0"

type Slots = ( addTodoDialog :: forall query. AddTodoDialog.Slot query Int
             , tagModal :: forall query. TagModal.Slot query Int
             )

_addTodoDialog = Proxy :: Proxy "addTodoDialog"
_tagModal = Proxy :: Proxy "tagModal"

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ Prop.class_ (ClassName "columns")]
    [ sidebarView state
    , panelsView state
    , if state.showAddTodoModal
      then HH.div_ [ HH.slot _addTodoDialog 0 AddTodoDialog.component 0 HandleAddTodo ]
      else HH.div_ []
    , case state.modalTarget of
        Just task -> HH.div_ [ HH.slot _tagModal 1 TagModal.component task HandleTags ]
        Nothing -> HH.div_ []
    ]

splitTodosByTitle :: Task -> Array Task -> Maybe { init :: Array Task, task :: Task, rest :: Array Task }
splitTodosByTitle task tasks =
  let { init: init, rest: rest } = span (\t -> t.title == task.title) tasks
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
        index <- findIndex (\t -> t.title == task.title) st.tasks
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

  HandleAddTodo AddTodoDialog.Canceled ->
    H.modify_ \st -> st { showAddTodoModal = false }

  HandleAddTodo (AddTodoDialog.TaskCreated task) ->
    H.modify_ \st -> st { tasks = cons task st.tasks
                        , showAddTodoModal = false
                        }

  HandleTags tagsModalOutput -> case tagsModalOutput of
    TagModal.Canceled -> H.modify_ \st -> st { modalTarget = Nothing }
    TagModal.TagsAdded task tags -> H.modify_ \st ->
      -- This is very similar to the DroppedOn case above
      let maybeTasks = do
            i <- findIndex (\t -> t.title == task.title) st.tasks
            modifyAt i (\t -> t { tags = t.tags <> tags}) st.tasks

          f :: Array Task -> State
          f tasks = st { tasks = tasks
                       , modalTarget = Nothing
                       }
      in maybe st f maybeTasks

  Noop -> pure unit
