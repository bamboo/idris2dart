module Dart.FFI.Elab.Types

import Data.List
import Data.List.Equalities

public export
DartName : Type
DartName = String

public export
data DartType : Type where
  NType : String -> DartType
  FType : List DartType -> DartType -> DartType
  GType : DartType -> (args : List DartType) -> {auto prf : NonEmpty args} -> DartType

public export
FromString DartType where
  fromString = NType

public export
Show DartType where
  show ty = case ty of
    NType n => n
    FType ps ret => concat $ intersperse " -> " $ show <$> (ps ++ [ret])
    GType t args => show t ++ show args

infixr 8 :->
public export
(:->) : DartType -> DartType -> DartType
(:->) ty (FType ps ret) = FType (ty :: ps) ret
(:->) p ret = FType [p] ret

infixl 9 :<>
public export
(:<>) : DartType -> DartType -> DartType
(:<>) (GType t args) arg = GType t (snoc args arg) @{SnocNonEmpty args arg}
(:<>) t arg = GType t [arg]

public export
data DartParameter
  = Positional DartName DartType
  | Named DartName DartType

infix 7 :?

namespace DartParameter
  public export
  (::) : DartName -> DartType -> DartParameter
  (::) = Positional

  public export
  (:?) : DartName -> DartType -> DartParameter
  (:?) = Named

public export
data DartDecl
  = Function DartType DartName (List DartParameter)
  | Val DartType DartName
  | Var DartType DartName
  | Effectful DartDecl
  | Static DartDecl
  | Class DartName (List DartDecl)
  | Enum DartName (List DartName)
  | Constructor DartName (List DartParameter)
  | Extends DartType
  | Generic (List DartName) DartDecl
  | Partial DartDecl
  | Const DartDecl

public export
Show DartDecl where
  show (Function _ n _) = "fun " ++ n
  show (Constructor n _) = "new " ++ show n
  show (Const d) = "const " ++ show d
  show (Val _ n) = "final " ++ n
  show (Var _ n) = "var " ++ n
  show (Effectful d) = "io $ " ++ show d
  show (Static d) = "static $ " ++ show d
  show (Class n _) = "class " ++ show n
  show (Enum n _) = "enum " ++ show n
  show (Extends t) = "extends " ++ show t
  show (Generic _ d) = "generic " ++ show d
  show (Partial d) = "partial " ++ show d

public export
record DartPackage where
  constructor MkDartPackage
  name : String
  declarations : List DartDecl

