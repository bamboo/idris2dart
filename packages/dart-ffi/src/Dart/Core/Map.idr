module Dart.Core.Map

import Dart.FFI.Elab
import Dart.Core

%language ElabReflection

%runElab importDart [
  package "dart:core" [
    generic ["k", "v"] $
      class' "MapEntry" [
        final "k" "key",
        final "v" "value"
      ]
  ]
]

namespace Map

  namespace FromIterable

    public export
    data Tag : Type where

    %inline
    public export
    key : {a : Type} -> {k : Type} -> Parameter Tag
    key = mkParameter "key" (a -> k)

    %inline
    public export
    value : {a : Type} -> {v : Type} -> Parameter Tag
    value = mkParameter "value" (a -> v)

    %inline
    public export
    NamedParameters : {a : Type} -> {k : Type} -> {v : Type} -> Type
    NamedParameters = Parameters [FromIterable.key {a = a} {k = k}, FromIterable.value {a = a} {v = v}]

  %inline
  export
  fromIterable : HasIO io
    => {k : Type}
    -> {v : Type}
    -> {iterable : Type}
    -> {a : Type}
    -> iterable
    -> IsAssignableFrom (Iterable a) iterable
    => (a -> k)
    -> (a -> v)
    -> io (Map k v)
  fromIterable iter fk fv = primIO $
    prim__dart_new
      (Map k v)
      "fromIterable"
      [iter]
      (the (FromIterable.NamedParameters {a = a} {k = k} {v = v}) [key @= fk, value @= fv])

  %inline
  public export
  fromPairs : {k : Type} -> {v : Type} -> Prelude.List (k, v) -> IO (Dart.Core.Map k v)
  fromPairs entries = Map.fromIterable (Dart.Core.List.fromList entries) fst snd

  %inline
  export
  new : HasIO io => {k : Type} -> {v : Type} -> io (Map k v)
  new = primIO $ prim__dart_new (Map k v) "" [] none

  %inline
  export
  put : HasIO io => {k : Type} -> k -> {v : Type} -> v -> Map k v -> io ()
  put k v m = primIO $ prim__dart_invoke "[]" [] [m, k, v] none
