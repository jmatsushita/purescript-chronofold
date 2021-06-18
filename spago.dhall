{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "benchotron"
  , "console"
  , "debug"
  , "effect"
  , "errors"
  , "foldable-traversable"
  , "foreign-generic"
  , "generics-rep"
  , "maybe"
  , "naturals"
  , "newtype"
  , "ordered-collections"
  , "profunctor-lenses"
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
