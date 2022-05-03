{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "fgl"
, dependencies =
  [ "arrays"
  , "catenable-lists"
  , "console"
  , "effect"
  , "enums"
  , "lazy"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "spec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
