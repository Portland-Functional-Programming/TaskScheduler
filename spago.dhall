{ name = "halogen-project"
, dependencies =
  [ "console", "debug", "effect", "halogen", "psci-support", "web-html" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
