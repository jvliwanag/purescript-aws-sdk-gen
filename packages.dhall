let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201021/packages.dhall sha256:55ebdbda1bd6ede4d5307fbc1ef19988c80271b4225d833c8d6fb9b6fb1aa6d8

let overrides = {=}

let additions =
      { dodo-printer =
        { dependencies =
          [ "aff"
          , "ansi"
          , "avar"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "minibench"
          , "node-child-process"
          , "node-fs-aff"
          , "node-process"
          , "psci-support"
          , "strings"
          ]
        , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
        , version = "v1.0.8"
        }
      , ps-cst =
        { dependencies =
          [ "ansi"
          , "console"
          , "dodo-printer"
          , "effect"
          , "generics-rep"
          , "node-fs-aff"
          , "node-path"
          , "psci-support"
          , "record"
          , "spec"
          , "strings"
          ]
        , repo = "https://github.com/purescript-codegen/purescript-ps-cst.git"
        , version = "5b0a078"
        }
      , cst-simple =
        { dependencies =
          [ "arrays"
          , "console"
          , "debug"
          , "effect"
          , "node-fs-aff"
          , "parsing"
          , "ps-cst"
          , "psci-support"
          , "spec"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/purescript-codegen/purescript-cst-simple.git"
        , version = "1dcd0fd"
        }
      }

in  upstream ⫽ overrides ⫽ additions
