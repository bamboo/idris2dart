module Main

import Dart.Core
import Dart.FFI
import Dart.FFI.Elab
import Data.String

%foreign """
Dart:defineGenericBox,

// A Dart generic type.
class Box<T> {
  final T value;
  Box(this.value);
  // A Dart generic member function.
  Box<U> map<U>(U Function(T) f) => Box<U>(f(value));
}

defineGenericBox() {}
"""
defineGenericBox : PrimIO ()

%language ElabReflection

%runElab importDart [
  package "" [
    generic ["a"] $
      class' "Box" [
        new "" ["value" :: "a"],
        final "a" "value",
        generic ["b"] $
          fun ("Box" :<> "b") "map" ["f" :: "a" :-> "b"]
      ]
  ]
]

main : IO ()
main = do
  box <- Box.new "foo"
  Core.print box
  Core.print (box @. map String.toUpper @. value)
  primIO defineGenericBox
