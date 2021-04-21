module Dart.FFI.Types

import public Dart.FFI.Constructors

||| Represents a Dart generic type.
public export
data GenericType : String -> List Type -> Type where [external]

||| Represents a Dart nullable type (T?).
export
data Nullable : a -> Type where [external]

infixl 9 @.

||| Object-oriented method invocation syntax.
%inline
public export
(@.) : a -> (a -> b) -> b
(@.) x f = f x

export
Cast a (Nullable a) where
  cast a = believe_me a
