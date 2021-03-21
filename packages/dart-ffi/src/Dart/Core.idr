module Dart.Core

import Dart.FFI.Constructors
import Dart.FFI.Upcast
import public Dart.FFI.Types
import public System.FFI

%inline
export
print : HasIO io => {a : Type} -> a -> io ()
print value = primIO $ prim__dart_invoke "print,dart:core" [] [value] Parameters.none

%inline
public export
Object : Type
Object = Struct "Object,dart:core" [("hashCode", Int)]

public export
IsAssignableFrom Object _ where

%inline
public export
Symbol : Type
Symbol = Struct "Symbol,dart:core" [("hashCode", Int)]

%inline
public export
Iterable : Type -> Type
Iterable element = Struct "Iterable,dart:core" [("first", element), ("last", element)]

%inline
public export
DartList : Type -> Type
DartList element = GenericType "List,dart:core" [element]

%inline
public export
DartMap : Type -> Type -> Type
DartMap key val = GenericType "Map,dart:core" [key, val]

namespace Object

  public export
  toString : HasIO io => {a : Type} -> a -> io String
  toString this = primIO $ prim__dart_invoke ".toString" [] [this] Parameters.none

%inline
public export
DartBool : Type
DartBool = Struct "bool,dart:core" [("hashCode", Int)]

export
%extern prim__dart_if : DartBool -> a -> a -> a

namespace DartBool

  %inline
  public export
  true : DartBool
  true = prim__dart_get_pure "true" Void

  %inline
  public export
  false : DartBool
  false = prim__dart_get_pure "false" Void

  %inline
  public export
  fromBool : Bool -> DartBool
  fromBool b = prim__dart_invoke_pure "==" [] [b, True] none

  %inline
  public export
  toBool : DartBool -> Bool
  toBool b = prim__dart_if b True False

%inline
export
prim__dart_eq : a -> a -> Bool
prim__dart_eq x y =
  let
    x' = the AnyPtr (believe_me x)
    y' = the AnyPtr (believe_me y)
  in toBool (prim__dart_invoke_pure "==" [] [x', y'] none)

%inline
export
Cast Bool DartBool where
  cast = fromBool

%inline
export
Cast DartBool Bool where
  cast = toBool

%inline
export
Eq DartBool where
  (==) x y = toBool (prim__dart_invoke_pure "==" [] [x, y] none)

export
Show DartBool where
  show b = show (toBool b)

%extern prim__dart_List_new : (element : Type) -> (1 x : %World) -> IORes (DartList element)

%extern prim__dart_List_empty : (element : Type) -> DartList element

%extern prim__dart_List_fromList : (element : Type) -> List element -> DartList element

namespace DartList

  %inline
  export
  empty : {element : Type} -> DartList element
  empty = prim__dart_List_empty element

  %inline
  export
  new : HasIO io => {element : Type} -> io (DartList element)
  new = primIO $ prim__dart_List_new element

  %inline
  export
  fromList : {element : Type} -> List element -> DartList element
  fromList l = prim__dart_List_fromList element l

  %inline
  export
  add : HasIO io => {element : Type} -> element -> DartList element -> io ()
  add e list = primIO $ prim__dart_invoke ".add" [] [list, e] none

namespace Map

  %inline
  export
  new : HasIO io => {key : Type} -> {val : Type} -> io (DartMap key val)
  new = primIO $ prim__dart_new (DartMap key val) "" [] none

  %inline
  export
  put : HasIO io => {key : Type} -> key -> {val : Type} -> val -> DartMap key val -> io ()
  put k v m = primIO $ prim__dart_invoke "[]" [] [m, k, v] none

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

  %inline
  public export
  (-) : Duration -> Duration -> Duration
  (-) x y = prim__dart_invoke_pure "-" [] [x, y] Parameters.none

  %inline
  public export
  (+) : Duration -> Duration -> Duration
  (+) x y = prim__dart_invoke_pure "+" [] [x, y] Parameters.none

export
data Nullable : a -> Type where [external]

namespace Nullable

  %inline
  export
  null : Nullable a
  null = prim__dart_get_pure "null" Void

  %inline
  export
  isNull : Nullable a -> Bool
  isNull a =
    let
      a' = the AnyPtr (believe_me a)
      null' = the AnyPtr (believe_me a)
    in toBool (prim__dart_invoke_pure "==" [] [a', null'] none)

  %inline
  export
  nullable : Lazy b -> (a -> b) -> Nullable a -> b
  nullable nullValue f a = if isNull a then nullValue else f (believe_me a)

  %inline
  export
  toMaybe : Nullable a -> Maybe a
  toMaybe = nullable Nothing Just

  %inline
  export
  traverse : Applicative f => (a -> f b) -> Nullable a -> f (Nullable b)
  traverse f a =
    if isNull a
      then pure (believe_me a)
      else believe_me (f (believe_me a))

export
Cast a (Nullable a) where
  cast a = believe_me a

public export
IsAssignableFrom (Iterable element) (DartList element) where

namespace Iterable

  %inline
  public export
  forEach : HasIO io => {element : Type} -> {iterable : Type} -> IsAssignableFrom (Iterable element) iterable => (element -> IO ()) -> iterable -> io ()
  forEach action iterable = primIO $ prim__dart_invoke ".forEach" [] [iterable, action] Parameters.none
