{ name = "halogen-project"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "halogen"
  , "psci-support"
  , "unsafe-coerce"
  , "web-html"
  , "simple-ajax"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
