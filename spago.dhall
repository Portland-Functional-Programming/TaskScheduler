{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "console"
  , "custom-element"
  , "debug"
  , "effect"
  , "halogen"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
