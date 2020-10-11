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
    %inline
    public export
    x : Param
    x = ("x", Int)

    %inline
    public export
    y : Param
    y = ("y", Int)

  %inline
  public export
  new : ParamList [New.x, New.y] -> IO Point
  new ps = primIO $ prim__dart_new Point [] ps

%inline
export
Callbacks : Type
Callbacks = Struct "Callbacks,./libsmall.dart" []

namespace Callbacks

  namespace New
    %inline
    public export
    one : Param
    one = ("one", Int -> Int)

    %inline
    public export
    two : Param
    two = ("two", Int -> IO Int)

  %inline
  public export
  new : ParamList [New.one, New.two] -> IO Callbacks
  new ps = primIO $ prim__dart_new Callbacks [] ps

  export
  %foreign "Dart:.callOne"
  callOne : Callbacks -> Int -> Int

  export
  %foreign "Dart:.callTwo"
  callTwo : Callbacks -> Int -> PrimIO Int

main : IO ()
main = do
  pt <- Point.new [
    x @= 42,
    y @= 37
  ]
  printLn pt

  cb <- Callbacks.new [
    one @= (\i => i * 2),
    two @= (\i => printLn i *> pure (i * 2))
  ]
  printLn (callOne cb 21)
  printLn !(primIO (callTwo cb 21))


