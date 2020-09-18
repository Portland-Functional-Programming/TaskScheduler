module Main where

import Prelude

import App.Button as Button
import App.ActivityList as Activity
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Activity.component unit body
