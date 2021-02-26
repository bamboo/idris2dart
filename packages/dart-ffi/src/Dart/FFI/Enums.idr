module Dart.FFI.Enums

import Dart.Core

public export
interface IsEnum a where

namespace Enum

  %inline
  export
  equals : IsEnum a => a -> a -> Bool
  equals = prim__dart_eq

export
IsEnum a => Eq a where
  (==) = Enum.equals
