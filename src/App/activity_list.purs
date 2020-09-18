module App.ActivityList where

import Prelude

import Control.Monad.State (state)
import Data.List (List, singleton)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (id_)
import Halogen.HTML.Properties as Prop
import Prelude as List

data Priority =
    High
    | Medium
    | Low


type Todo =
  {
    panel :: String,
    name :: String,
    priority :: Priority
  }

type TodoNow = {
    todos :: Maybe (List Todo) 
}
type ActivityInventoryList = {
    todos :: Maybe (Array Todo) 
}

initialTodos :: ActivityInventoryList
initialTodos =
  { todos: Just ([
    {
      panel : "backlog",
      name : "Finish planning",
      priority : High
    }
    , {
      panel : "backlog",
      name : "next",
      priority : High
      }
    ])
  }

type State =
    {
        activityInventoryList :: ActivityInventoryList,
        todoNow :: Maybe TodoNow,
        selectedTodo :: Maybe Todo
    }

data Action = Noop

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { activityInventoryList: initialTodos, todoNow: Nothing, selectedTodo: Nothing   }
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


panelsView :: forall cs m. State -> HH.HTML cs m
panelsView state =
  HH.div [ Prop.classes [ClassName "column"]] 
    [
      HH.section [ Prop.class_ (ClassName "section")] 
      [
        HH.div [ Prop.id_"Todo"] 
          [
            HH.div [ Prop.class_ (ClassName "container")] 
              [ HH.div [ Prop.class_ (ClassName "panel"), Prop.id_ "activityInventoryList" ] 
                  [ HH.h1_ [ HH.text "Activity Inventory List"]]
                , HH.div [ Prop.class_ (ClassName "panel"), Prop.id_ "todoToday" ] 
                    [ HH.h1_ [ HH.text "Todo Today"]]
                , HH.div [ Prop.class_ (ClassName "panel"), Prop.id_ "selectedTodo" ] 
                    [ HH.h1_ [ HH.text "Currently doing" ]]
              ]
          ]
      ]
    ]  

listView :: forall cs m. State -> HH.HTML cs m
listView state =
  case state.activityInventoryList.todos of
    Just todos ->
      HH.ul_ (
        map (\todo -> HH.li [] [HH.text  todo.name]) todos)
    Nothing ->
      HH.ul_ []
    

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div [ Prop.class_ (ClassName "columns")] 
    [ sidebarView state
    , panelsView state
    ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Noop ->
    H.modify_ \st -> st 
