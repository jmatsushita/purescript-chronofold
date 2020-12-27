{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "errors"
  , "foldable-traversable"
  , "generics-rep"
  , "maybe"
  , "naturals"
  , "newtype"
  , "ordered-collections"
  , "psci-support"
  , "quickcheck"
  , "spec"
  , "spec-mocha"
  , "spec-quickcheck"
  , "strings"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
