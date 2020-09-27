module Dart.Core

import System.FFI

mutual
  public export
  Object : Type
  Object = Struct "Object,dart:core" [("hashCode", Int)]

  public export
  Symbol : Type
  Symbol = Struct "Symbol,dart:core" [("hashCode", Int)]

  public export
  Iterable : Type -> Type
  Iterable ty = Struct "Iterable,dart:core" [("first", ty), ("last", ty)]

  public export
  DartList : Type -> Type
  DartList ty = Struct "List,dart:core" [("length", Int), ("first", ty), ("last", ty)]

  public export
  DartMap : Type -> Type -> Type
  DartMap key val = Struct "Map,dart:core" [("keys", Iterable key), ("values", Iterable val)]

namespace Object
  %foreign "Dart:.toString"
  prim__toString : Object -> PrimIO String

  public export
  toString : HasIO io => a -> io String
  toString o = primIO $ prim__toString (believe_me o)

namespace List
  export
  %foreign "Dart:List.empty,dart:core"
  empty : DartList _

  %foreign "Dart:List,dart:core"
  prim__new : PrimIO (DartList _)

  %inline
  export
  new : HasIO io => io (DartList _)
  new = primIO prim__new

  %foreign "Dart:.add"
  prim__add : DartList ty -> ty -> PrimIO ()

  export
  add : HasIO io => {0 ty : Type} -> ty -> DartList ty -> io ()
  add e list = primIO $ prim__add list e

namespace Map
  %foreign "Dart:Map,dart:core"
  prim__new : PrimIO (DartMap _ _)

  %inline
  export
  new : HasIO io => io (DartMap _ _)
  new = primIO prim__new

  %foreign "Dart:[]"
  prim__put : DartMap key val -> key -> val -> PrimIO ()

  export
  put : HasIO io => {0 key : Type} -> key -> {0 val : Type} -> val -> DartMap key val -> io ()
  put k v m = primIO $ prim__put m k v

