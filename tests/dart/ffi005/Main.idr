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
  static Point from({$.int x, $.int y}) => Point(x: x, y: y);
}" [
  ("x", Int),
  ("y", Int)
]

Show Point where
  show pt = show (the Int (getField pt "x"), the Int (getField pt "y"))

namespace Point

  namespace From

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
  from : Parameters [From.x, From.y] -> IO Point
  from ps = primIO $ prim__dart_invoke "Point.from" [] ps

main : IO ()
main = do
  pt <- Point.from [
    x @= 42,
    y @= 37
  ]
  printLn pt