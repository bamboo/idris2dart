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
DartBool : Type
DartBool = Struct "bool,dart:core" [("hashCode", Int)]

export
%extern prim__dart_true : DartBool

export
%extern prim__dart_false : DartBool

export
%extern prim__dart_eq : a -> a -> DartBool

export
%extern prim__dart_if : DartBool -> a -> a -> a

namespace DartBool
  %inline
  public export
  true : DartBool
  true = prim__dart_true

  %inline
  public export
  false : DartBool
  false = prim__dart_false

  %inline
  public export
  fromBool : Bool -> DartBool
  fromBool b = prim__dart_eq b True

  %inline
  public export
  toBool : DartBool -> Bool
  toBool b = prim__dart_if b True False

export
Cast Bool DartBool where
  cast = fromBool

export
Cast DartBool Bool where
  cast = toBool

export
Eq DartBool where
  (==) x y = toBool (prim__dart_eq x y)

export
Show DartBool where
  show b = show (toBool b)

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
  ("inMicroseconds", Int),
  ("inSeconds", Int)
]

namespace Duration

  public export
  %foreign "Dart:const Duration.zero,dart:core"
  zero : Duration

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
  inSeconds : Duration -> Int
  inSeconds = (`getField` "inSeconds")

  %inline
  public export
  inMilliseconds : Duration -> Int
  inMilliseconds = (`getField` "inMilliseconds")

  %inline
  public export
  inMicroseconds : Duration -> Int
  inMicroseconds = (`getField` "inMicroseconds")

  %foreign "Dart:Duration_minus,
Duration_minus($.Duration x, $.Duration y) => x - y;"
  prim__Duration_minus : Duration -> Duration -> Duration

  %foreign "Dart:Duration_plus,
Duration_plus($.Duration x, $.Duration y) => x + y;"
  prim__Duration_plus : Duration -> Duration -> Duration

  public export
  (-) : Duration -> Duration -> Duration
  (-) = prim__Duration_minus

  public export
  (+) : Duration -> Duration -> Duration
  (+) = prim__Duration_plus
