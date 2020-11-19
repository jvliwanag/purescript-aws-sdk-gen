{ name = "gen"
, dependencies =
  [ "aff"
  , "console"
  , "cst-simple"
  , "effect"
  , "foreign-generic"
  , "node-fs-aff"
  , "prelude"
  , "psci-support"
  , "ps-cst"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
