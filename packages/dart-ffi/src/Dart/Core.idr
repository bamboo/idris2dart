module Dart.Core

import System.FFI

%foreign "Dart:print,dart:core"
prim__print : AnyPtr -> PrimIO ()

export
print : HasIO io => a -> io ()
print a = primIO $ prim__print (believe_me a)

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

public export
%extern prim__dart_list_new : (elTy : Type) -> (1 x : %World) -> IORes (DartList elTy)

namespace List
  %inline
  export
  new : HasIO io => {elTy : Type} -> io (DartList elTy)
  new = primIO $ prim__dart_list_new elTy

  export
  %foreign "Dart:.add"
  prim__list_add : DartList AnyPtr -> AnyPtr -> PrimIO ()

  %inline
  export
  add : HasIO io => {0 elTy : Type} -> elTy -> DartList elTy -> io ()
  add e list = primIO $ prim__list_add (believe_me list) (believe_me e)

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

%inline
public export
Duration : Type
Duration = Struct "Duration,dart:core" [
  ("inMilliseconds", Int),
  ("inMicroseconds", Int)
]

namespace Duration

  %foreign "Dart:const Duration.microsecondsPerMinute,dart:core"
  public export
  microsecondsPerMinute : Int

  %foreign "Dart:const Duration.microsecondsPerSecond,dart:core"
  public export
  microsecondsPerSecond : Int

  %foreign "Dart:const Duration.microsecondsPerMillisecond,dart:core"
  public export
  microsecondsPerMillisecond : Int

  %inline
  public export
  inMilliseconds : Duration -> Int
  inMilliseconds = (`getField` "inMilliseconds")

  %inline
  public export
  inMicroseconds : Duration -> Int
  inMicroseconds = (`getField` "inMicroseconds")
