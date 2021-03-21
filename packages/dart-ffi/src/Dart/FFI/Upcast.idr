module Dart.FFI.Upcast

||| Instances of type [subType] can be [upcast] to type [superType].
public export
interface IsAssignableFrom superType subType where

%inline
export
upcast : {0 subType : Type}
  -> {0 superType : Type}
  -> subType
  -> IsAssignableFrom superType subType
  => superType
upcast sub = believe_me sub

%inline
public export
IsAssignableFrom superType subType => Cast subType superType where
  cast sub = upcast sub

||| Every type is assignable from itself.
export
IsAssignableFrom a a where

-- public export
-- (IsAssignableFrom a b, IsAssignableFrom b c) => IsAssignableFrom a c where
