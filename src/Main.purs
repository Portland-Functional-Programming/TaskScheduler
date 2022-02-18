module Main where

import Prelude

import App.ActivityList as Activity
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Halogen.CustomElement as CustomElement
import App.Component.AddTodoDialog as TodoDialog

main :: Effect Unit
main = CustomElement.define "halogen-todo" TodoDialog.component
-- main = HA.runHalogenAff do
--   body <- HA.awaitBody
--   runUI Activity.component unit body
