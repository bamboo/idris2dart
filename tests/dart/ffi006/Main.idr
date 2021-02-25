{- Tests for typed native Dart lists -}
module Main

import Dart.Core
import Dart.FFI

%foreign "Dart:code,
code() {}
printStrings($.List<$.String> list) => $.print(list);
printInts($.List<$.int> list) => $.print(list);
printIntegers($.List<$.BigInt> list) => $.print(list);
"
prim__code : PrimIO ()

importForeignCode : IO ()
importForeignCode = primIO prim__code

printStrings : DartList String -> IO ()
printStrings es = primIO $ prim__dart_invoke "printStrings" [es] none

printInts : DartList Int -> IO ()
printInts es = primIO $ prim__dart_invoke "printInts" [es] none

printIntegers : DartList Integer -> IO ()
printIntegers es = primIO $ prim__dart_invoke "printIntegers" [es] none

into : {element : Type} -> DartList element -> List element -> IO (DartList element)
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
