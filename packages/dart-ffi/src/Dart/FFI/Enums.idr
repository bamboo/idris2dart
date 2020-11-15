module Dart.FFI.Enums

public export
interface IsEnum a where

export
%foreign "Dart:prim__enum_equals,
$.bool prim__enum_equals($.Object x, $.Object y) => x == y;"
prim__enum_equals : AnyPtr -> AnyPtr -> Bool

namespace Enum
  %inline
  public export
  equals : IsEnum a => a -> a -> Bool
  equals x y = prim__enum_equals (believe_me x) (believe_me y)

export
IsEnum a => Eq a where
  (==) = Enum.equals
