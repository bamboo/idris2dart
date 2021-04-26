module Dart.FFI.Elab.Implementation

import public Dart.FFI
import public Dart.FFI.Elab.Types
import public Data.Either
import public Data.List1
import public Data.Maybe
import public Data.String
import public Language.Reflection

titleCase : String -> String
titleCase s = toUpper (substr 0 1 s) ++ substr 1 (length s) s

mkTy : Name -> TTImp -> ITy
mkTy = MkTy EmptyFC EmptyFC

primVal : Constant -> TTImp
primVal = IPrimVal EmptyFC

publicExport : List FnOpt -> ITy -> Decl
publicExport = IClaim EmptyFC MW Public

export
publicExportHint : ITy -> Decl
publicExportHint = publicExport [Hint False]

publicExportInline : ITy -> Decl
publicExportInline = publicExport [Inline]

patClause : TTImp -> TTImp -> Clause
patClause = PatClause EmptyFC

var : Name -> TTImp
var = IVar EmptyFC

app : TTImp -> TTImp -> TTImp
app = IApp EmptyFC

namedApp : TTImp -> Name -> TTImp -> TTImp
namedApp = INamedApp EmptyFC

pi : Name -> TTImp -> TTImp -> TTImp
pi n v = IPi EmptyFC MW ExplicitArg (Just n) v

implicitPi : Name -> TTImp -> TTImp -> TTImp
implicitPi n v = IPi EmptyFC MW ImplicitArg (Just n) v

bindVar : String -> TTImp
bindVar = IBindVar EmptyFC

inNamespace : Namespace -> List Decl -> Decl
inNamespace ns = INamespace EmptyFC ns

def : Name -> List Clause -> Decl
def = IDef EmptyFC

bindParameter : TTImp -> String -> TTImp
bindParameter ps p = `(~ps ~(bindVar p))

parameterized : TTImp -> List String -> TTImp
parameterized n ps = foldl bindParameter n ps

simpleDef : Name -> List String -> TTImp -> Decl
simpleDef n ps val =
  def n [
    patClause (parameterized (var n) ps) val
  ]

defExport : Name -> List String -> TTImp -> TTImp -> List Decl
defExport n ps ty val = [
  publicExportInline (mkTy n ty),
  simpleDef n ps val
]

dartInvoke : String -> TTImp -> TTImp -> TTImp -> TTImp
dartInvoke foreignName typeArgs positional named =
  `(prim__dart_invoke ~(primVal (Str foreignName)) ~typeArgs ~positional ~named)

dartInvokePure : String -> TTImp -> TTImp -> TTImp -> TTImp
dartInvokePure foreignName typeArgs positional named =
  `(prim__dart_invoke_pure ~(primVal (Str foreignName)) ~typeArgs ~positional ~named)

dartNew : TTImp -> String -> TTImp -> TTImp -> TTImp
dartNew ty ctorName positional named =
  `(prim__dart_new ~ty ~(primVal (Str ctorName)) ~positional ~named)

dartNewConst : TTImp -> String -> TTImp -> TTImp -> TTImp
dartNewConst ty ctorName positional named =
  `(prim__dart_new_const ~ty ~(primVal (Str ctorName)) ~positional ~named)

listWithVars : List String ->  TTImp
listWithVars vars = foldr (\v, acc => `(~(var (UN v)) :: ~acc)) `(Nil) vars

positionalOrNamed : DartParameter -> Either (DartName, DartType) (DartName, DartType)
positionalOrNamed (Positional n ty) = Left (n, ty)
positionalOrNamed (Named n ty) = Right (n, ty)

consArrow : TTImp -> TTImp -> TTImp
consArrow ty acc = `(~ty -> ~acc)

varOf : DartName -> TTImp
varOf n = var $
  case reverse (toList (split (=='.') n)) of
    (n :: ns@(_ :: _)) => NS (MkNS ns) (UN n)
    _ => UN n

TypeParameters : Type
TypeParameters = List (String, TTImp)

elabType' : TypeParameters -> DartType -> TTImp
elabType' ts = \case
  FType ps ret => foldr (\p, acc => `(~p -> ~acc)) (elabType' ts ret) (elabType' ts <$> ps)
  GType t args => foldl app (elabType' ts t) (elabType' ts <$> args)
  NType n => case n of
    "int" => `(Int)
    "Int" => `(Int)
    "String" => `(String)
    "void" => `(())
    "()" => `(())
    "bool" => `(DartBool)
    "Bool" => `(DartBool)
    "double" => `(Double)
    "Double" => `(Double)
    _ => case lookup n ts of
      Nothing => varOf n
      Just _ => bindVar n

elabType : DartType -> TTImp
elabType = elabType' []

packageQualifiedName : {auto p : DartPackage} -> String -> String
packageQualifiedName n =
  if length p.name == 0
    then n
    else n ++ "," ++ p.name

withMemberName : String -> String -> String
withMemberName qname member =
  case split (== ',') qname of
    q ::: p :: _ => q ++ "." ++ member ++ "," ++ p
    _ => qname ++ "." ++ member

none : TTImp
none = `(Parameters.none)

record Invocation where
  constructor MkInvocation
  hasIO : Bool
  invoke : (positional : TTImp) -> (named : TTImp) -> TTImp

functionType : TTImp -> TypeParameters -> TTImp
functionType ty ps = foldr (\(pn, pty), acc => pi (UN pn) pty acc) ty ps

functionType' : TTImp -> TypeParameters -> TypeParameters -> TTImp
functionType' ty implicitPs ps =
  let signature = functionType ty ps
  in foldr (\(pn, pty), acc => implicitPi (UN pn) pty acc) signature implicitPs

defParameter : TypeParameters -> Namespace -> (DartName, DartType) -> List Decl
defParameter tps tagNS (name, ty) =
  let
    tag = var (NS tagNS (UN "Tag"))
    tag' = foldl app tag (var . UN . fst <$> tps)
  in
    defExport (UN name) []
      (functionType' `(Parameter ~(tag')) tps [])
      `(mkParameter ~(primVal (Str name)) ~(elabType ty))

tagDataType : TypeParameters -> List Decl
tagDataType tps = `[
  public export data Tag : ~(functionType' `(Type) [] tps) where
]

qualifiedNamedApp : TypeParameters -> Namespace -> String -> TTImp
qualifiedNamedApp tps ns n =
  let qname = var (NS ns (UN n))
  in foldl (\acc, t => let n = UN t in namedApp acc n (var n)) qname (fst <$> tps)

consParameter : TypeParameters -> Namespace -> TTImp -> String -> TTImp
consParameter tps ns acc n =
  let name = qualifiedNamedApp tps ns n
  in `(~name :: ~acc)

parameterDeclarationsFor : TypeParameters -> Namespace -> Namespace -> List (DartName, DartType) -> List Decl
parameterDeclarationsFor tps ns tagNS named =
  let
    namedNames = fst <$> named
    namedParameters = foldl (consParameter tps tagNS) `(Nil) namedNames
  in [
    inNamespace ns $
      tagDataType tps
      ++ concatMap (defParameter tps tagNS) named
      ++ `[
        %inline
        public export
        NamedParameters : ~(functionType' `(Type) tps [])
        NamedParameters = Parameters ~(namedParameters)
      ]
  ]

elabFunctionWithNamedParameters
  : List String
 -> Invocation
 -> TypeParameters
 -> (thisTy : Maybe TTImp)
 -> TTImp
 -> String
 -> List DartParameter
 -> List (DartName, DartType)
 -> List (DartName, DartType)
 -> List Decl
elabFunctionWithNamedParameters parentNS invocation typeParams thisTy ty idrisName ps positional named =
  let
    topNS = titleCase idrisName
    ns = MkNS [topNS]
    tagNS = MkNS (topNS :: parentNS)
    namedParametersTy = qualifiedNamedApp typeParams tagNS "NamedParameters"
    fn = UN idrisName
    returnType = if invocation.hasIO then `(~(bindVar "io") ~(ty)) else ty
    thisParam = ("this",) <$> thisTy
    params = (map (elabType' typeParams) <$> positional) ++ [("ps", namedParametersTy)] ++ toList thisParam
    paramNames = fst <$> params
    posArgs = listWithVars (toList (fst <$> thisParam) ++ (fst <$> positional))
    sig = functionType' returnType typeParams params
  in parameterDeclarationsFor typeParams ns tagNS named ++
    if invocation.hasIO
      then defExport fn paramNames `(HasIO ~(bindVar "io") => ~sig) `(primIO $ ~(invocation.invoke posArgs `(ps)))
      else defExport fn paramNames sig (invocation.invoke posArgs `(ps))

elabFunctionWithPositionalParameters : Invocation
  -> TypeParameters
  -> Maybe TTImp
  -> TTImp
  -> (idrisName : String)
  -> List DartParameter
  -> List (DartName, DartType)
  -> List Decl
elabFunctionWithPositionalParameters invocation typeParams thisTy ty idrisName ps positional =
  let
    fn = UN idrisName
    returnType = if invocation.hasIO then `(~(bindVar "io") ~(ty)) else ty
    thisParam = ("this",) <$> thisTy
    params = (map (elabType' typeParams) <$> positional) ++ toList thisParam
    paramNames = fst <$> params
    posArgs = listWithVars (toList (fst <$> thisParam) ++ (fst <$> positional))
    sig = functionType' returnType typeParams params
  in if invocation.hasIO
    then defExport fn paramNames `(HasIO ~(bindVar "io") => ~sig) `(primIO ~(invocation.invoke posArgs none))
    else defExport fn paramNames sig (invocation.invoke posArgs none)

elabFunction'
  : List String
 -> Invocation
 -> TypeParameters
 -> (thisTy : Maybe TTImp)
 -> TTImp
 -> (idrisName : String)
 -> List DartParameter
 -> List Decl
elabFunction' ns invocation typeParams thisTy ty n ps =
  let (positional, named) = partitionEithers (positionalOrNamed <$> ps)
  in if null named
    then elabFunctionWithPositionalParameters invocation typeParams thisTy ty n ps positional
    else elabFunctionWithNamedParameters ns invocation typeParams thisTy ty n ps positional named

ioInvocationOf : String -> List String -> Invocation
ioInvocationOf fn typeArgs = MkInvocation {
  hasIO = True,
  invoke = dartInvoke fn (listWithVars typeArgs)
}

pureInvocationOf : String -> List String -> Invocation
pureInvocationOf fn typeArgs = MkInvocation {
  hasIO = False,
  invoke = dartInvokePure fn (listWithVars typeArgs)
}

elabFunction
  : {auto p : DartPackage}
 -> (hasIO : Bool)
 -> DartType
 -> DartName
 -> List DartParameter
 -> List Decl
elabFunction hasIO ty n ps =
  let
    fn = packageQualifiedName n
    invocation = if hasIO then ioInvocationOf fn [] else pureInvocationOf fn []
  in elabFunction' [] invocation [] Nothing (elabType ty) n ps

elabConstructorOf : List String -> TTImp -> TypeParameters -> String -> List DartParameter -> Invocation -> List Decl
elabConstructorOf ns ty typeParams n ps invocation =
  elabFunction'
    ns
    invocation
    typeParams
    Nothing
    ty
    (if length n == 0 then "new" else n)
    ps

elabVal' : TypeParameters -> Maybe TTImp -> DartType -> DartName -> String -> List Decl
elabVal' typeParams (Just thisTy) ty n fn =
  defExport (UN n) ["this"] (functionType' (elabType ty) typeParams [("thisTy", thisTy)])
    `(prim__dart_get_pure ~(primVal (Str fn)) ~(var (UN "this")))
elabVal' typeParams Nothing ty n fn =
  defExport (UN n) [] (elabType ty)
    `(prim__dart_get_pure ~(primVal (Str fn)) Void)

elabVal : DartType -> DartName -> String -> List Decl
elabVal = elabVal' [] Nothing

elabVar : TypeParameters -> Maybe TTImp ->  DartType -> DartName -> String -> List Decl
elabVar typeParams thisTy ty n fn =
  let
    io = bindVar "io"
    ty' = elabType ty
    get = UN n
    set = UN ("set" ++ titleCase n)
    getTy = `(~io ~ty')
    setTy = `(~io ())
  in case thisTy of
    Just thisTy =>
      defExport get ["this"]
        `(HasIO ~io => ~(functionType' getTy typeParams [("this", thisTy)]))
        `(primIO $ prim__dart_get ~(primVal (Str fn)) ~(var (UN "this")))
      ++ defExport set ["value", "this"]
        `(HasIO ~io => ~(functionType' setTy typeParams [("value", ty'), ("this", thisTy)]))
        `(ignore $ primIO $ prim__dart_set ~(primVal (Str fn)) ~(var (UN "value")) ~(var (UN "this")))
    Nothing =>
      defExport get []
        `(HasIO ~io => ~getTy)
        `(primIO $ prim__dart_get ~(primVal (Str fn)) Void)
      ++ defExport set ["value"]
        `(HasIO ~io => (value : ~ty') -> ~setTy)
        `(ignore $ primIO $ prim__dart_set ~(primVal (Str fn)) ~(var (UN "value")) Void)

simpleNameOf : DartType -> Maybe String
simpleNameOf = \case
  NType n => Just n
  GType t _ => simpleNameOf t
  FType _ _ => Nothing

elabTypeParams : List String -> List (String, TTImp)
elabTypeParams tps = (,`(Type)) <$> tps

elabClassMember : List String -> String -> List (String, TTImp) -> TTImp -> DartDecl -> List (Decl)
elabClassMember ns qName typeParams thisTy d = case d of
  Static (Effectful (Function ty n ps)) =>
    elabFunction' ns (ioInvocationOf (withMemberName qName n) []) typeParams Nothing (elabType ty) n ps
  Generic funTypeParams (Static (Effectful (Function ty n ps))) =>
    elabFunction' ns (ioInvocationOf (withMemberName qName n) funTypeParams) (elabTypeParams funTypeParams) Nothing (elabType ty) n ps
  Effectful (Function ty n ps) =>
    elabFunction' ns (ioInvocationOf ("." ++ n) []) typeParams (Just thisTy) (elabType ty) n ps
  Generic funTypeParams (Effectful (Function ty n ps)) =>
    elabFunction' ns (ioInvocationOf ("." ++ n) funTypeParams) (typeParams ++ elabTypeParams funTypeParams) (Just thisTy) (elabType ty) n ps
  Function ty n ps =>
    elabFunction' ns (pureInvocationOf ("." ++ n) []) typeParams (Just thisTy) (elabType ty) n ps
  Generic funTypeParams (Function ty n ps) =>
    elabFunction' ns (pureInvocationOf ("." ++ n) funTypeParams) (typeParams ++ elabTypeParams funTypeParams) (Just thisTy) (elabType ty) n ps
  Static (Function ty n ps) =>
    elabFunction' ns (pureInvocationOf (withMemberName qName n) []) typeParams Nothing (elabType ty) n ps
  Generic funTypeParams $ Static (Function ty n ps) =>
    elabFunction' ns (pureInvocationOf (withMemberName qName n) funTypeParams) (elabTypeParams funTypeParams) Nothing (elabType ty) n ps
  Static (Val ty n) =>
    elabVal ty n (withMemberName qName n)
  Val ty n =>
    elabVal' typeParams (Just thisTy) ty n ("." ++ n)
  Static (Var ty n) =>
    elabVar [] Nothing ty n (withMemberName qName n)
  Var ty n =>
    elabVar typeParams (Just thisTy) ty n ("." ++ n)
  Constructor n ps =>
    elabConstructorOf ns thisTy typeParams n ps (MkInvocation {hasIO = True, invoke = dartNew thisTy n})
  Const $ Constructor n ps =>
    elabConstructorOf ns thisTy typeParams n ps (MkInvocation {hasIO = False, invoke = dartNewConst thisTy n})
  _ => []

elabEnumMember : {auto p : DartPackage} -> TTImp -> String -> String -> List (Decl)
elabEnumMember enumTy dartType n =
  defExport (UN n) [] enumTy
    `(prim__dart_get_pure ~(primVal (Str (packageQualifiedName (dartType ++ "." ++ n)))) Void)

elabClass : {auto p : DartPackage} -> DartName -> List String -> List DartDecl -> Elab ()
elabClass n ps members = do
  let qName = packageQualifiedName n
  when (not (null members)) $
    declare [
      inNamespace (MkNS [n])
        let
          typeParams = elabTypeParams ps
          thisTy = foldl (\ps, p => `(~ps ~(var (UN p)))) (var (UN n)) ps
        in concatMap (elabClassMember [n] qName typeParams thisTy) members
    ]

elabPackageDecl : {auto p : DartPackage} -> DartDecl -> Elab ()
elabPackageDecl = \case
  Generic funTypeParams (Effectful (Function ty n ps)) =>
    declare $ elabFunction' [] (ioInvocationOf (packageQualifiedName n) funTypeParams) (elabTypeParams funTypeParams) Nothing (elabType ty) n ps
  Effectful (Function ty fn ps) =>
    declare $ elabFunction True ty fn ps
  Function ty fn ps =>
    declare $ elabFunction False ty fn ps
  Val ty n =>
    declare $ elabVal ty n (packageQualifiedName n)
  Var ty n =>
    declare $ elabVar [] Nothing ty n (packageQualifiedName n)
  Generic ps $ Class n members =>
    elabClass n ps members
  Class n members =>
    elabClass n [] members
  Enum n members =>
    when (not (null members)) $
      declare [
        inNamespace (MkNS [n])
          (concatMap (elabEnumMember (var (UN n)) n) members)
      ]
  Partial d =>
    elabPackageDecl d
  d => fail $ "Unsupported Dart definition: " ++ show d

lookupIsAssignableFromConstructor : Elab Name
lookupIsAssignableFromConstructor = do
  [(n, _)] <- getType `{{IsAssignableFrom}}
    | _ => fail "IsAssignableFrom is not in scope."
  [c] <- getCons n
    | _ => fail "IsAssignableFrom should have a single constructor!"
  pure c

record ExtendsInfo where
  constructor MkExtendsInfo
  className : DartName
  superType : DartType

extendsInfo : DartName -> DartDecl -> Maybe ExtendsInfo
extendsInfo className (Extends t) = Just (MkExtendsInfo className t)
extendsInfo _ _ = Nothing

defStruct : {auto p : DartPackage} -> String -> List Decl
defStruct n =
  defExport (UN n) [] `(Type)
    `(Struct ~(primVal (Str (packageQualifiedName n))) [])

defGeneric : {auto p : DartPackage} -> String -> List String -> List Decl
defGeneric n ps =
  let ty = `(Type)
  in defExport (UN n) ps (functionType ty ((,ty) <$> ps))
    `(GenericType ~(primVal (Str (packageQualifiedName n))) ~(listWithVars ps))

elabTypeDecl : {auto p : DartPackage} -> DartDecl -> List Decl
elabTypeDecl = \case
  Class n members => defStruct n
  Enum n members => defStruct n
  Generic ps (Class n _) => defGeneric n ps
  _ => []

elabTypeDeclExtends : {auto p : DartPackage} -> DartDecl -> List ExtendsInfo
elabTypeDeclExtends = \case
  Class n members => mapMaybe (extendsInfo n) members -- TODO: Enum => IsEnum
  Partial d => elabTypeDeclExtends d
  _ => []

elabExtends : Name -> ExtendsInfo -> List Decl
elabExtends isAssignableFromCtor e =
  let n = UN (e.className ++ "Extends" ++ fromMaybe "" (simpleNameOf e.superType))
  in [
    publicExportHint (mkTy n `(IsAssignableFrom ~(elabType e.superType) ~(var (UN e.className)))),
    simpleDef n [] (var isAssignableFromCtor)
  ]

elabPackage : DartPackage -> Elab ()
elabPackage p = do
  -- Declare all `IsAssignableFrom` / `IsEnum` constraints
  let extensions = concatMap elabTypeDeclExtends p.declarations
  when (not (null extensions)) $ do
    isAssignableFromCtor <- lookupIsAssignableFromConstructor
    for_ extensions (declare . elabExtends isAssignableFromCtor)

  -- Only then, the actual functions
  for_ p.declarations elabPackageDecl

export
importDart : List DartPackage -> Elab ()
importDart packages = do
  -- Declare all foreign types first
  for_ packages \p =>
    for_ p.declarations (declare . elabTypeDecl)
  for_ packages elabPackage
