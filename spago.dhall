{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "halogen"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "tuples"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
