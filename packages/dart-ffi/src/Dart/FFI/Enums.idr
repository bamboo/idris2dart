module Dart.FFI.Enums

import Dart.Core

public export
interface IsEnum a where

namespace Enum
  %inline
  public export
  equals : IsEnum a => a -> a -> Bool
  equals x y = toBool (prim__dart_eq x y)

export
IsEnum a => Eq a where
  (==) = Enum.equals
