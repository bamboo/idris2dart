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
Iterable element = GenericType "Iterable,dart:core" [element]

%inline
public export
List : Type -> Type
List element = GenericType "List,dart:core" [element]

%inline
public export
Map : Type -> Type -> Type
Map k v = GenericType "Map,dart:core" [k, v]

%inline
public export
Set : Type -> Type
Set e = GenericType "Set,dart:core" [e]

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

%extern prim__dart_List_new : (element : Type) -> (1 x : %World) -> IORes (Dart.Core.List element)

%extern prim__dart_List_empty : (element : Type) -> Dart.Core.List element

%extern prim__dart_List_fromList : (element : Type) -> Prelude.List element -> Dart.Core.List element

namespace List

  %inline
  export
  empty : {element : Type} -> Dart.Core.List element
  empty = prim__dart_List_empty element

  %inline
  export
  new : HasIO io => {element : Type} -> io (Dart.Core.List element)
  new = primIO $ prim__dart_List_new element

  %inline
  export
  fromList : {element : Type} -> Prelude.List element -> Dart.Core.List element
  fromList l = prim__dart_List_fromList element l

  %inline
  export
  add : HasIO io => {element : Type} -> element -> Dart.Core.List element -> io ()
  add e list = primIO $ prim__dart_invoke ".add" [] [list, e] none

  %inline
  export
  into : HasIO io => {element : Type} -> Dart.Core.List element -> UpcastList element -> io (Dart.Core.List element)
  into result es = do
    traverse_ (`add` result) (toList es)
    pure result

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

public export
IsAssignableFrom (Iterable element) (Dart.Core.List element) where

namespace Iterable

  %inline
  public export
  forEach : HasIO io => {element : Type} -> {iterable : Type} -> IsAssignableFrom (Iterable element) iterable => (element -> IO ()) -> iterable -> io ()
  forEach action iterable = primIO $ prim__dart_invoke ".forEach" [] [iterable, action] Parameters.none

namespace Nullable

  %inline
  export
  null : Nullable a
  null = prim__dart_get_pure "null" Void

  %inline
  export
  isNull : Nullable a -> Bool
  isNull a = toBool $
    prim__dart_invoke_pure "==" [] [
      the AnyPtr (believe_me a),
      the AnyPtr (believe_me (Nullable.null {a = Void}))
    ] none

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
      then pure Nullable.null
      else cast <$> f (believe_me a)
