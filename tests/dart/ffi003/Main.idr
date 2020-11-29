module Main

import Dart.Core
import Dart.FFI

%inline
export
Point : Type
Point = Struct "Point,
// All foreign names must be fully qualified and
// the following import can always be assumed to exist:
// import 'dart:core' as $;
class Point {
  final $.int x;
  final $.int y;
  Point({this.x, this.y});
  Point.of(this.x, this.y);
}" [
  ("x", Int),
  ("y", Int)
]

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
  new ps = primIO $ prim__dart_new Point [] ps

%inline
export
Callbacks : Type
Callbacks = Struct "Callbacks,
class Callbacks {
  $.Object Function($.Object) x;
  $.Object Function($.Object) y;
  $.bool enabled;
  Callbacks({this.x, this.y, this.enabled});
  $.Object callX($.Object arg) => x(arg);
  $.Object callY($.Object arg) => y(arg);
}" [
  ("enabled", DartBool)
]

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
    enabled : Parameter Tag
    enabled = MkParameter "enabled" DartBool Tag

  %inline
  public export
  new : Parameters [Callbacks.New.x, Callbacks.New.y, Callbacks.New.enabled] -> IO Callbacks
  new ps = primIO $ prim__dart_new Callbacks [] ps

  export
  %foreign "Dart:.callX"
  callX : Callbacks -> Int -> Int

  export
  %foreign "Dart:.callY"
  callY : Callbacks -> Int -> PrimIO Int

||| A pure foreign function with a bool argument.
%foreign "Dart:ifBool,
$.Object ifBool($.bool condition, $.Object then, $.Object else_) {
  return condition ? then : else_;
}"
ifBool : DartBool -> Int -> Int -> Int

main : IO ()
main = do
  pt <- Point.new [
    x @= 42,
    y @= 37
  ]
  printLn pt

  cb <- Callbacks.new [
    x @= (\i => i * 2),
    y @= (\i => printLn i *> pure (i * 2)),
    enabled @= true
  ]
  printLn (callX cb 21)
  printLn !(primIO (callY cb 21))

  -- Dart bool
  printLn (the DartBool (cb `getField` "enabled"))
  printLn (ifBool true 1 2)
  printLn (ifBool false 1 2)
  pure ()
