||| https://idris2.readthedocs.io/en/latest/ffi/ffi.html translated to the
||| ElabReflection based Dart FFI.
module Main

import Dart.FFI.Elab

%language ElabReflection

%runElab importDart [
  package "./libsmall.dart" [

    fun "Int" "add" ["x" :: "Int", "y" :: "Int"],

    io "Int" "addWithMessage" ["s" :: "String", "x" :: "Int", "y" :: "Int"],

    io "String" "applyFn" [
      "c" :: "String",
      "i" :: "Int",
      "f" :: "String" :-> "Int" :-> "IO" :<> "String"
    ],

    class' "Point" [
      final "Int" "x",
      final "Int" "y",
      new "" ["x" :: "Int", "y" :: "Int"],
      io "void" "moveTo" ["x" :: "Int", "y" :: "Int"],
      static $ io "Point" "origin" []
    ]
  ]
]

Show Point where
  show pt = show (pt @. x, pt @. y)

pluralise : String -> Int -> String
pluralise str x =
  show x ++ " " ++
    if x == 1
      then str
      else str ++ "s"

pluraliseIO : String -> Int -> IO String
pluraliseIO str x = pure (pluralise str x)

main : IO ()
main = do
  -- Global functions
  printLn (add 70 24)
  addWithMessage "Sum" 70 24 >>= printLn
  applyFn "Biscuit" 10 pluraliseIO >>= putStrLn
  applyFn "Tree" 1 pluraliseIO >>= putStrLn

  -- Classes
  pt <- Point.new 0 1
  printLn pt
  pt @. moveTo 4 5
  printLn pt

  -- Static functions
  printLn !Point.origin