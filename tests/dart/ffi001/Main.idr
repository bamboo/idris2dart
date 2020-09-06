||| https://idris2.readthedocs.io/en/latest/ffi/ffi.html
module Main

import System.FFI

libsmall : String -> String
libsmall fn = "Dart:" ++ fn ++ ",./libsmall.dart"

||| A pure foreign function.
%foreign (libsmall "add")
add : Int -> Int -> Int

||| An effectful foreign function.
%foreign (libsmall "addWithMessage")
prim__addWithMessage : String -> Int -> Int -> PrimIO Int

addWithMessage : HasIO io => String -> Int -> Int -> io Int
addWithMessage s x y = primIO $ prim__addWithMessage s x y

||| An effectful foreign function that takes a pure callback.
%foreign (libsmall "applyFn")
prim__applyFn : String -> Int -> (String -> Int -> String) -> PrimIO String

applyFn : HasIO io => String -> Int -> (String -> Int -> String) -> io String
applyFn c i f = primIO $ prim__applyFn c i f

||| An effectful foreign function that takes an effectful callback.
%foreign (libsmall "applyFn")
prim__applyFnIO :
  String -> Int -> (String -> Int -> PrimIO String) -> PrimIO String

applyFnIO : HasIO io =>
  String -> Int -> (String -> Int -> IO String) -> io String
applyFnIO c i f = primIO $ prim__applyFnIO c i (\s, i => toPrim $ f s i)

pluralise : String -> Int -> String
pluralise str x =
  show x ++ " " ++
    if x == 1
      then str
      else str ++ "s"

pluraliseIO : String -> Int -> IO String
pluraliseIO str x = pure (pluralise str x)

||| A static member of the dart:core library.
DateTime : Type
DateTime = Struct "DateTime,dart:core" [("hour", Int)]

%foreign "Dart:DateTime.parse,dart:core"
prim__parseDateTime : String -> PrimIO DateTime

namespace DateTime
  export
  parse : HasIO io => String -> io DateTime
  parse s = primIO $ prim__parseDateTime s

  export
  hour : DateTime -> Int
  hour dt = dt.getField "hour"

Point : Type
Point = Struct "Point,./libsmall.dart" [("x", Int), ("y", Int)]

%foreign (libsmall "Point")
prim__mkPoint : Int -> Int -> PrimIO Point

mkPoint : Int -> Int -> IO Point
mkPoint x y = primIO $ prim__mkPoint x y

Show Point where
  show pt = show (the Int (pt.getField "x"), the Int (pt.getField "y"))

main : IO ()
main = do
  printLn (add 70 24)
  addWithMessage "Sum" 70 24 >>= printLn
  applyFn "Biscuit" 10 pluralise >>= putStrLn
  applyFn "Tree" 1 pluralise >>= putStrLn
  applyFnIO "Biscuit" 10 pluraliseIO >>= putStrLn
  applyFnIO "Tree" 1 pluraliseIO >>= putStrLn
  moonLanding <- DateTime.parse "1969-07-20 20:18:04Z"
  printLn moonLanding.hour
  pt <- mkPoint 0 1
  printLn pt
  pt.setField "x" (the Int 2)
  pt.setField "y" (the Int 3)
  printLn pt
  pure ()
