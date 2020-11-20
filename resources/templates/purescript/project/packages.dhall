let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201021/packages.dhall sha256:55ebdbda1bd6ede4d5307fbc1ef19988c80271b4225d833c8d6fb9b6fb1aa6d8

let overrides = {=}

let additions =
      { aws-request =
        { dependencies =
          [ "aff"
          , "console"
          , "datetime"
          , "effect"
          , "exceptions"
          , "foreign"
          , "foreign-object"
          , "js-date"
          , "untagged-union"
          , "prelude"
          , "psci-support"
          ]
        , repo =
            "https://github.com/purescript-aws-sdk/purescript-aws-request.git"
        , version = "afe76f23d3e3dec2e84ac2e63c446f54fa037403"
        }
      }

in  upstream ⫽ overrides ⫽ additions
