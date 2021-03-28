module Dart.FFI.Types

||| Represents a Dart generic type.
public export
data GenericType : String -> List Type -> Type where [external]


infixl 9 @.

||| Object-oriented method invocation syntax.
%inline
public export
(@.) : a -> (a -> b) -> b
(@.) = flip ($)
