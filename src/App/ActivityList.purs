module App.ActivityList where

import Prelude

import Control.Monad.State (state)
import Data.Array (fromFoldable)
import Data.List (List, singleton)
import Data.Maybe (Maybe(..), fromJust)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML (HTML(..))
import Halogen.HTML as HH
import Halogen.HTML as HTML
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (id_)
import Halogen.HTML.Properties as Prop
import Web.HTML.Event.DragEvent as DE
import Prelude as List

import Partial.Unsafe (unsafePartial)
import Debug.Trace (trace)
import Unsafe.Coerce (unsafeCoerce)

data Priority = High
              | Medium
              | Low

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

component :: forall q i o m. H.Component HH.HTML q i o m
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
        -- , HE.onDragEnter (\de -> )
         , HE.onDrop (\_ -> Just $ DroppedOn panel)
         ]
         [ HH.h1_ [ HH.text panel.name ]
         , listView panel.todos
         ]

listView :: forall cs m. Array Todo -> HH.HTML cs Action
listView todos =
  HH.div [ Prop.class_ (ClassName "itemContainer")] (map todoView  todos)

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

getPanel :: Todo -> Panel
getPanel = unsafeCoerce unit

removeTodo :: Todo -> Array Panel -> Array Panel
removeTodo = unsafeCoerce unit

addTodo :: Todo -> Panel -> Panel
addTodo = unsafeCoerce unit

replacePanel :: Panel -> Array Panel -> Array Panel
replacePanel = unsafeCoerce unit

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Dragging todo -> H.modify_ \st -> st { transitioning = Just todo }
  DroppedOn panel -> H.modify_ \st ->
    let todo = unsafePartial (fromJust st.transitioning)
        --sourcePanel = getPanel todo
        panels' = removeTodo todo st.panels
        destPanel' = addTodo todo panel
        panels'' = replacePanel destPanel' panels'
    in st { transitioning = Nothing
          , panels = panels''
          }
        
