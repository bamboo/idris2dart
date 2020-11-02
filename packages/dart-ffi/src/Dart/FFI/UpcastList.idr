module Dart.FFI.UpcastList

import Dart.Core
import Dart.FFI.Upcast

public export
data UpcastList : Type -> Type where
  MkUpcastList : List elTy -> UpcastList elTy

namespace UpcastList
  %inline
  export
  Nil : UpcastList _
  Nil = MkUpcastList []

  %inline
  export
  (::) : IsAssignableFrom elTy ty => ty -> UpcastList elTy -> UpcastList elTy
  (::) e (MkUpcastList es) = MkUpcastList (upcast e :: es)

  export
  toList : UpcastList elTy -> List elTy
  toList (MkUpcastList es) = es

  export
  into : DartList elTy -> UpcastList elTy -> IO (DartList elTy)
  into result es = do
    traverse (`add` result) (toList es)
    pure result

