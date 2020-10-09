module App.ActivityList where

import Prelude

import Data.Array (filter)
import Data.Maybe (Maybe(..), maybe)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as Prop
import Web.HTML.Event.DragEvent as DE
import Web.Event.Event (Event, preventDefault)
import Effect.Class (class MonadEffect)

data Priority = High
              | Medium
              | Low

derive instance eqPriority :: Eq Priority

data Panel = ActivityInventoryList
           | TodoToday
           | TodoNow

derive instance eqPanel :: Eq Panel

showPanel :: Panel -> String
showPanel ActivityInventoryList = "Activity Inventory List"
showPanel TodoToday = "Todo Today"
showPanel TodoNow = "Todo Now"

type Todo =
  { panel :: Panel
  , name :: String
  , priority :: Priority
  }

type TodoNow =
  { todos :: Maybe (Array Todo)
  } 

type ActivityInventoryList =
  { todos :: Maybe (Array Todo)
  }

initialTodos :: Array Todo
initialTodos = [ { panel : ActivityInventoryList
                 , name : "Finish planning",
                   priority : High
                 }
               , { panel : ActivityInventoryList
                 , name : "next",
                   priority : Medium
                 }
               , { panel : ActivityInventoryList
                 , name : "trivial task",
                   priority : Medium
                 }
               ]

initialPanels :: Array Panel
initialPanels = [ ActivityInventoryList
                , TodoNow
                , TodoToday
                ]

type State =
    { panels :: Array Panel
    , todos :: Array Todo
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
                          , todos: initialTodos
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

panelsView :: forall cs. State -> HH.HTML cs Action
panelsView state =
  HH.div [ Prop.classes [ClassName "column"]]
    [
      HH.section [ Prop.class_ (ClassName "section")]
      [
        HH.div [ Prop.id_"Todo"]
          [
            HH.div [ Prop.class_ (ClassName "container")]
            (map (panelsListView state) state.panels)
          ]
      ]
    ]

panelsListView :: forall cs. State -> Panel -> HH.HTML cs Action
panelsListView state panel =
  HH.div [ Prop.class_ (ClassName "panel"), Prop.id_ "activityInventoryList"
         , HE.onDragOver (\de -> Just $ PreventDefault (DE.toEvent de) Noop)
         , HE.onDrop (\de -> Just $ PreventDefault (DE.toEvent de) (DroppedOn panel))
         ]
         [ HH.h1_ [ HH.text (showPanel panel) ]
         , listView (filter (\todo -> todo.panel == panel) state.todos)
         ]

listView :: forall cs. Array Todo -> HH.HTML cs Action
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

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Dragging todo -> H.modify_ \st -> st { transitioning = Just todo }
  DroppedOn panel -> H.modify_ \st ->
    let maybeTodos = do
          todo <- st.transitioning
          Just $ map (\todo2 -> if todo == todo2
                                then todo { panel = panel }
                                else todo2)
                     st.todos
    in maybe st
             (\todos -> st { transitioning = Nothing
                           , todos = todos
                           }
             )
             maybeTodos
  PreventDefault e next -> do
    H.liftEffect $ preventDefault e
    handleAction next
  Noop -> pure unit
