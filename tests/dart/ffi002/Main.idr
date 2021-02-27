module Main

import Dart.FFI

libsmall : String -> String
libsmall fn = "Dart:" ++ fn ++ ",./libsmall.dart"

%inline
export
Point : Type
Point = Struct "Point,./libsmall.dart" [("x", Int), ("y", Int)]

Show Point where
  show pt = show (the Int (getField pt "x"), the Int (getField pt "y"))

namespace Point

  namespace New

    data Tag : Type where

    %inline
    public export
    x : Parameter Tag
    x = MkParameter "x" Int Tag

    %inline
    public export
    y : Parameter Tag
    y = MkParameter "y" Int Tag

  %inline
  public export
  new : Parameters [New.x, New.y] -> IO Point
  new ps = primIO $ prim__dart_new Point "" [] ps

  %inline
  public export
  of' : Int -> Int -> IO Point
  of' x y = primIO $ prim__dart_new Point "of" [x, y] none

%inline
export
Callbacks : Type
Callbacks = Struct "Callbacks,./libsmall.dart" []

namespace Callbacks

  namespace New

    data Tag : Type where

    %inline
    public export
    x : Parameter Tag
    x = MkParameter "x" (Int -> Int) Tag

    %inline
    public export
    y : Parameter Tag
    y = MkParameter "y" (Int -> IO Int) Tag

  %inline
  public export
  new : Parameters [Callbacks.New.x, Callbacks.New.y] -> IO Callbacks
  new ps = primIO $ prim__dart_new Callbacks "" [] ps

  export
  %foreign "Dart:.callX"
  callX : Callbacks -> Int -> Int

  export
  %foreign "Dart:.callY"
  callY : Callbacks -> Int -> PrimIO Int

main : IO ()
main = do
  pt <- Point.new [
    x @= 42,
    y @= 37
  ]
  printLn pt

  printLn !(Point.of' 11 31)

  cb <- Callbacks.new [
    x @= (\i => i * 2),
    y @= (\i => printLn i *> pure (i * 2))
  ]
  printLn (callX cb 21)
  printLn !(primIO (callY cb 21))
