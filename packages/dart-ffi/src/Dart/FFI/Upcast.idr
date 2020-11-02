module Dart.FFI.Upcast

 
||| Values of type [b] can be [upcast] to [a].
public export
interface IsAssignableFrom a b where
  %inline
  upcast : b -> a
  upcast b = believe_me b

public export
IsAssignableFrom a a where

-- public export
-- (IsAssignableFrom a b, IsAssignableFrom b c) => IsAssignableFrom a c where
