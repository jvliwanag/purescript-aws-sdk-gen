{ name = "gen"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "foreign-generic"
  , "node-fs-aff"
  , "prelude"
  , "psci-support"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
