module Dart.FFI.Constructors

import public System.FFI
import public Data.HVect
import public Data.List.Elem
import public Dart.FFI.Upcast

public export
data Parameter : (tag : Type) -> Type where
  MkParameter : (name : String) -> (ty : Type) -> (0 tag : Type) -> Parameter tag

%inline
public export
mkParameter : {tag : Type} -> String -> Type -> Parameter tag
mkParameter name ty = MkParameter name ty tag

%inline
public export
typeOf : Parameter tag -> Type
typeOf (MkParameter _ ty _) = ty

public export
data Argument : Parameter tag -> Type where
  Assign : {0 tag : Type} -> {ty : Type} -> (name : String) -> (value : ty) -> Argument (MkParameter name ty tag)

%inline
public export
Schema : (tag : Type) -> Type
Schema tag = List (Parameter tag)

public export
data Parameters : Schema tag -> Type where
  Nil : {0 schema : Schema tag} -> Parameters schema
  (::) : {0 p : Parameter tag} -> {0 schema : Schema tag}
      -> Argument p -> Parameters schema -> {auto 0 isValid : Elem p schema} -> Parameters schema

namespace Parameters
  %inline
  public export
  none : Parameters {tag = Void} []
  none = []

infix 1 @=
infix 1 @=>

%inline
public export
(@=) : {tag : Type} -> (p : Parameter tag) -> typeOf p -> Argument p
(@=) (MkParameter name _ _) value = Assign {tag = tag} name value

||| Upcast assignment.
%inline
public export
(@=>) : {tag : Type} -> (p : Parameter tag) -> ty -> {auto isAssignable : typeOf p `IsAssignableFrom` ty} -> Argument p
(@=>) (MkParameter name _ _) value = Assign {tag = tag} name (upcast value)

public export
%extern prim__dart_new : (ty : Type) -> HVect positional -> Parameters named -> (1 x : %World) -> IORes ty

public export
%extern prim__dart_invoke : {0 res : Type} -> {positionalTys : Vect k Type} -> (foreignName : String) -> (positional : HVect positionalTys) -> Parameters named -> (1 x : %World) -> IORes res
