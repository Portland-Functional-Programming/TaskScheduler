{ name = "halogen-project"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "halogen"
  , "psci-support"
  , "unsafe-coerce"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
