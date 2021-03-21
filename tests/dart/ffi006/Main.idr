{- Tests for typed native Dart lists -}
module Main

import Dart.Core
import Dart.FFI

%foreign """
Dart:code,
code() {}
printStrings($.List<$.String> list) => $.print(list);
printInts($.List<$.int> list) => $.print(list);
printIntegers($.List<$.BigInt> list) => $.print(list);
"""
prim__code : PrimIO ()

importForeignCode : IO ()
importForeignCode = primIO prim__code

%inline
printListVia : String -> {a : Type} -> DartList a -> IO ()
printListVia f es = primIO $ prim__dart_invoke f [] [es] none

printStrings : DartList String -> IO ()
printStrings = printListVia "printStrings"

printInts : DartList Int -> IO ()
printInts = printListVia "printInts"

printIntegers : DartList Integer -> IO ()
printIntegers = printListVia "printIntegers"

into : DartList element -> List element -> IO (DartList element)
into list es = do
  traverse_ (flip add list) es
  pure list

main : IO ()
main = do
  importForeignCode
  printStrings empty
  printStrings (fromList ["do", "re"])
  printStrings !(into !new ["mi", "fa"])
  printInts empty
  printInts (fromList {element = Int} [1..10])
  printInts !(into !new [10..1])
  printIntegers empty
  printIntegers (fromList {element = Integer} [1..10])
  printIntegers !(into !new [10..1])
