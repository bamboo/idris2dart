module Dart.FFI.UpcastList

import Dart.Core
import Dart.FFI.Upcast

public export
data UpcastList : Type -> Type where
  MkUpcastList : Prelude.List element -> UpcastList element

namespace UpcastList

  %inline
  export
  Nil : UpcastList _
  Nil = MkUpcastList []

  %inline
  export
  (::) : IsAssignableFrom element ty => ty -> UpcastList element -> UpcastList element
  (::) e (MkUpcastList es) = MkUpcastList (upcast e :: es)

  %inline
  export
  toList : UpcastList element -> Prelude.List element
  toList (MkUpcastList es) = es

  %inline
  export
  into : HasIO io => {element : Type} -> Core.List element -> UpcastList element -> io (Core.List element)
  into result es = do
    traverse_ (`add` result) (toList es)
    pure result

