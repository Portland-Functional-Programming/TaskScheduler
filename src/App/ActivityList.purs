module App.ActivityList where

import Prelude

import Control.Monad.State (state)
import Data.Array (fromFoldable, delete, cons, snoc, span, tail, any, head, (:), singleton)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML (HTML(..))
import Halogen.HTML as HH
import Halogen.HTML as HTML
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (id_)
import Halogen.HTML.Properties as Prop
import Web.HTML.Event.DragEvent as DE
import Web.Event.Event (Event, preventDefault)
import Effect.Class (class MonadEffect)
import Prelude as List

import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

data Priority = High
              | Medium
              | Low

derive instance eqPriority :: Eq Priority

type Todo =
  { name :: String
  , priority :: Priority
  }

type TodoNow =
  { todos :: Maybe (Array Todo)
  } 

type ActivityInventoryList =
  { todos :: Maybe (Array Todo)
  }

type Panel =
  { name :: String
  , todos :: Array Todo
  }

initialTodos :: Array Todo
initialTodos = [ { name : "Finish planning",
                   priority : High
                 }
               , { name : "next",
                   priority : Medium
                 }
               , { name : "trivial task",
                   priority : Medium
                 }
               ]

initialPanels :: Array Panel
initialPanels = [ { name: "Activity Inventory List"
                  , todos: initialTodos
                  }
                , { name: "Todo Today"
                  , todos: []
                  }
                , { name: "Currently doing"
                  , todos: []
                  }
                ]

type State =
    { panels :: Array Panel
    , todoNow :: Maybe TodoNow
    , selectedTodo :: Maybe Todo
    , transitioning :: Maybe Todo
    }

data Action = Dragging Todo
            | DroppedOn Panel
            | PreventDefault Event Action
            | Noop

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { panels: initialPanels
                          , todoNow: Nothing
                          , selectedTodo: Nothing
                          , transitioning: Nothing}
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

panelsView :: forall cs m. State -> HH.HTML cs Action
panelsView state =
  HH.div [ Prop.classes [ClassName "column"]]
    [
      HH.section [ Prop.class_ (ClassName "section")]
      [
        HH.div [ Prop.id_"Todo"]
          [
            HH.div [ Prop.class_ (ClassName "container")]
            (map panelsListView state.panels)
          ]
      ]
    ]

panelsListView :: forall cs m. Panel -> HTML.HTML cs Action
panelsListView panel =
  HH.div [ Prop.class_ (ClassName "panel"), Prop.id_ "activityInventoryList"
         , HE.onDragOver (\de -> Just $ PreventDefault (DE.toEvent de) Noop)
         , HE.onDrop (\de -> Just $ PreventDefault (DE.toEvent de) (DroppedOn panel))
         ]
         [ HH.h1_ [ HH.text panel.name ]
         , listView panel.todos
         ]

listView :: forall cs m. Array Todo -> HH.HTML cs Action
listView todos =
  HH.div [ Prop.class_ (ClassName "itemContainer")] (map todoView todos)

todoView :: forall cs. Todo -> HH.HTML cs Action
todoView todo =
  HH.li
    [ Prop.class_ $ ClassName "item"
    , Prop.attr (AttrName "style")  $ "background-color: " <> priorityToColor todo.priority
    , Prop.attr (AttrName "draggable") "true"
    , HE.onDragStart \_ -> Just $ Dragging todo
    ] [ HH.text  todo.name
      ]

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
    ]

removeTodoFromPanel :: Todo -> Panel -> Panel
removeTodoFromPanel todo panel@{ todos: todos } = panel { todos = delete todo todos }

inPanel :: Todo -> Panel -> Boolean
inPanel todo@{ name: name' } panel = any (\{ name: name } -> name == name') panel.todos

splitPanelsByTodo :: Todo -> Array Panel -> Maybe { init :: Array Panel, panel :: Panel, rest :: Array Panel }
splitPanelsByTodo todo panels =
  let { init: init, rest: rest } = span (inPanel todo >>> not) panels
      maybePanel = head rest
      rest' = fromMaybe [] (tail rest)
  in map (\panel -> {init, panel, rest: rest'}) maybePanel

removeTodo :: Todo -> Array Panel -> Array Panel
removeTodo todo = map (removeTodoFromPanel todo)

addTodo :: Todo -> Panel -> Panel
addTodo todo panel@{ todos: todos } = panel { todos = snoc todos todo }

replacePanel :: Panel -> Array Panel -> Maybe (Array Panel)
replacePanel panel@{ name: name } panels =
  let { init: init, rest: rest } = span (\{ name: name' } -> name == name') panels
  in map (\tail -> init <> singleton panel <> tail) (tail rest)

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Dragging todo -> H.modify_ \st -> st { transitioning = Just todo }
  DroppedOn panel -> H.modify_ \st ->
    let maybePanels = do
          todo <- st.transitioning
          {init, panel: srcPanel, rest} <- splitPanelsByTodo todo st.panels
          let srcPanel' = removeTodoFromPanel todo srcPanel
              panels = init <> singleton srcPanel' <> rest
              panels' = map (\panel' -> if panel.name == panel'.name
                                        then panel' { todos = cons todo (panel'.todos) }
                                        else panel')
                            panels
          Just panels'
    in maybe st
             (\panels -> st { transitioning = Nothing
                            , panels = panels
                            }
             )
             maybePanels
  PreventDefault e next -> do
    H.liftEffect $ preventDefault e
    handleAction next
  Noop -> pure unit
