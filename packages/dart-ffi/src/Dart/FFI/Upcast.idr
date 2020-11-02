module Dart.FFI.Upcast
 
||| Instances of type [subclass] can be [upcast] to type [superclass].
public export
interface IsAssignableFrom superclass subclass where
  %inline
  upcast : subclass -> superclass
  upcast sub = believe_me sub

public export
IsAssignableFrom a a where

-- public export
-- (IsAssignableFrom a b, IsAssignableFrom b c) => IsAssignableFrom a c where
