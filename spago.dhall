{ name = "gen"
, dependencies =
  [ "aff"
  , "console"
  , "cst-simple"
  , "effect"
  , "foreign-generic"
  , "node-fs-aff"
  , "prelude"
  , "ps-cst"
  , "psci-support"
  , "spec"
  , "strings-extra"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
