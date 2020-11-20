{ name = "aws-{{MODULE_NAME_LOWER}}"
, dependencies = [ "aws-request" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
