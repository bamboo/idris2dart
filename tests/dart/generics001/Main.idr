{- Tests for typed native Dart lists -}
module Main

import Dart.Core
import Dart.FFI
import Data.String

%inline
Box : Type -> Type
Box a = GenericType """
Box,
// A Dart generic type.
class Box<T> {
  final T value;
  Box(this.value);
  // A Dart generic member function.
  Box<U> map<U>(U Function(T) f) => Box<U>(f(value));
}
""" [a]

namespace Box

  %inline
  export
  new : HasIO io => {a : Type} -> a -> io (Box a)
  new value = primIO $ prim__dart_new (Box a) "" [value] none

  %inline
  export
  get : {a : Type} -> Box a -> a
  get box = prim__dart_get_pure ".value" box

  %inline
  export
  map : {a : Type} -> {b : Type} -> (a -> b) -> Box a -> Box b
  map f this = prim__dart_invoke_pure ".map" [b] [this, f] none

main : IO ()
main = do
  box <- Box.new "foo"
  Core.print box
  Core.print (box @. map String.toUpper @. get)
