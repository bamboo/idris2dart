module Dart.FFI

import public System.FFI
import public Data.HVect
import public Data.List.Elem

%inline
public export
Param : Type
Param = (String, Type)

%inline
public export
typeOf : Param -> Type
typeOf = snd

public export
data Arg : Param -> Type where
  Assign : {ty : Type} -> (key : String) -> (value : ty) -> Arg (key, ty)

%inline
public export
ParamSchema : Type
ParamSchema = List Param

public export
data ParamList : ParamSchema -> Type where
  Nil : {0 schema : ParamSchema} -> ParamList schema
  (::) : {0 param : Param} -> {0 schema : ParamSchema}
      -> Arg param -> ParamList schema -> {auto 0 isValid : Elem param schema} -> ParamList schema

||| Values of type [b] can be [upcast] to [a].
public export
interface IsAssignableFrom a b where
  %inline
  upcast : b -> a
  upcast b = believe_me b

IsAssignableFrom a a where

-- (IsAssignableFrom a b, IsAssignableFrom b c) => IsAssignableFrom a c where

infix 1 @=
infix 1 @=>

%inline
public export
(@=) : (p : Param) -> typeOf p -> Arg p
(@=) (name, _) value = Assign name value

||| Upcast assignment.
%inline
public export
(@=>) : (p : Param) -> ty -> {auto isAssignable : typeOf p `IsAssignableFrom` ty} -> Arg p
(@=>) (name, _) value = Assign name (upcast value)

public export
%extern prim__dart_new : (ty : Type) -> HVect positional -> ParamList named -> (1 x : %World) -> IORes ty
