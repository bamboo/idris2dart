module SimpleFFI.Renderer

import SimpleFFI.Spec
import Data.Either
import Data.List
import Data.SortedMap
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.String

emptyLine : Doc ()
emptyLine = hardline <+> hardline

stringLit : String -> Doc ()
stringLit = dquotes . pretty

sepByLines : List (Doc ()) -> Doc ()
sepByLines ds@(d::_) = foldl1 (\acc, d => acc <+> emptyLine <+> d) ds
sepByLines [] = emptyDoc

indented : Doc () -> Doc ()
indented d = nest 2 (hardline <+> d) <+> hardline

inline : Doc ()
inline = pretty "%inline"

pubExport : Doc ()
pubExport = pretty "public export"

export_ : Doc ()
export_ = pretty "export" <+> hardline

typeHeader : Doc () -> Doc ()
typeHeader n = vcat [
  inline,
  pubExport,
  n <++> colon <++> pretty "Type"
]

arr : Doc ()
arr = pretty " ->"

funType : List (Doc ()) -> Doc ()
funType = hsep . punctuate arr

namespaced : String -> Doc () -> Doc ()
namespaced n body = pretty ("namespace " ++ n) <+> indented body

prettyType : DartType -> Doc ()
prettyType ty = case ty of
  BoolType => pretty "DartBool"
  IntType => pretty "Int"
  StringType => pretty "String"
  DoubleType => pretty "Double"
  VoidType => pretty "()"
  NamedType n => pretty n
  ListType a => parens (pretty "DartList" <++> prettyType a)
  FunctionType ps ret =>
    let sig = (prettyType <$> ps) ++ [pretty "IO" <++> prettyType ret]
    in parens (funType sig)

structField : (String, DartType) -> Doc ()
structField (n, ty) =
  parens $ hsep $ punctuate comma $ [stringLit n, prettyType ty]

fieldList : List (String, DartType) -> Doc ()
fieldList [] = pretty "[]"
fieldList fs =
  enclose "[" "]"
    (indented . vsep . punctuate comma $ (structField <$> fs))

foreignNameOf : {auto lib : Lib} -> String -> String
foreignNameOf name = name ++ "," ++ lib.package

typeDecl : {auto lib : Lib} -> String -> List (String, DartType) -> Doc ()
typeDecl name fields =
  let
    name' = pretty name
    foreignTypeName = dquotes (pretty (foreignNameOf name))
    struct = pretty "Struct" <++> foreignTypeName <++> fieldList fields
  in
    typeHeader name' <+> hardline <+> name' <++> equals <++> struct

fields : Class -> List Field
fields c = mapMaybe field c.members
  where
    field : Member -> Maybe Field
    field (FieldMember f) = Just f
    field _ = Nothing

nameAndType : Field -> (String, DartType)
nameAndType (Var n ty) = (n, ty)
nameAndType (Final n ty) = (n, ty)

typeDeclFor : {auto lib : Lib} -> Declaration -> Doc ()
typeDeclFor (ClassDecl c) =
  typeDecl c.name (nameAndType <$> fields c)
typeDeclFor (EnumDecl n fs) =
  typeDecl n []

typeDeclsForLib : LibDef -> List (Doc ())
typeDeclsForLib libDef =
  let lib = libDef.lib
  in typeDeclFor <$> libDef.declarations

this : Doc ()
this = pretty "this"

ioUnit : Doc ()
ioUnit = pretty "IO ()"

capitalize : String -> String
capitalize s =
  let cs = unpack s
  in pack ((toUpper <$> take 1 cs) ++ drop 1 cs)

setter : String -> String -> DartType -> Doc ()
setter owner field ty =
  let
    n = pretty ("set" ++ capitalize field)
    field' = pretty field
    ty' = prettyType ty
    val = pretty "value"
  in
    export_
      <+> n <++> colon <++> funType [ty', pretty owner, ioUnit] <+> hardline
      <+> n <++> val <++> this <++> equals
      <++> pretty "setField" <++> this <++> dquotes field' <++> val

getter : String -> String -> DartType -> Doc ()
getter owner field ty =
  let
    field' = pretty field
    ty' = prettyType ty
  in
    export_
      <+> field' <++> colon <++> funType [pretty owner, ty'] <+> hardline
      <+> field' <++> this <++> equals
      <++> pretty "getField" <++> this <++> dquotes field'

foreign' : Doc ()
foreign' = pretty "%foreign"

foreign : String -> Doc ()
foreign n = foreign' <++> dquotes (pretty ("Dart:" ++ n))

prettyParameter : Parameter -> Doc ()
prettyParameter (P n ty) = parens (pretty n <++> colon <++> prettyType ty)
prettyParameter _ = emptyDoc

partitionParameters : List Parameter -> (List (String, DartType), List (String, DartType))
partitionParameters ps = partitionEithers . flip map ps $ \case
  P n ty => Left (n, ty)
  N n ty => Right (n, ty)

parameterNames : List Parameter -> List (Doc ())
parameterNames ps = flip mapMaybe ps $ \case
  P n _ => Just (pretty n)
  _ => Nothing

parameters' : Doc ()
parameters'= pretty "NamedParameters"

namedParameterDeclarationsFor : String -> String -> List (String, DartType) -> Doc ()
namedParameterDeclarationsFor typeName _ [] = emptyDoc
namedParameterDeclarationsFor typeName ns ps = namespaced ns (sepByLines (tag :: (decl <$> ps)) <+> hardline <+> schema)
  where
    qualified : String -> Doc ()
    qualified n = pretty (typeName ++ "." ++ ns ++ "." ++ n)

    tag : Doc ()
    tag = pretty "data Tag : Type where"

    schema : Doc ()
    schema = vcat [
      inline,
      pubExport,
      parameters' <++> colon <++> pretty "Type",
      parameters' <++> equals <++> pretty "Parameters" <++> list ((\(n, _) => qualified n) <$> ps)
    ]

    decl : (String, DartType) -> Doc ()
    decl (n, ty) = vcat [
      inline,
      pubExport,
      pretty n <++> colon <++> pretty "Parameter" <++> qualified "Tag",
      pretty n <++> equals <++> pretty "mkParameter" <++> hsep [stringLit n, prettyType ty]
    ]

ctorPrimsFor : {auto lib : Lib} -> {auto c : Class} -> String -> List Parameter -> List (Doc ())
ctorPrimsFor n ps =
  let
    funName = if length n == 0 then "new" else n
    namedParameterNS = capitalize funName
    ret' = pretty c.name
    (positionalPs, namedPs) = partitionParameters ps
    positionalPs' = (\(n, ty) => parens (pretty n <++> colon <++> prettyType ty)) <$> positionalPs
    psNames = pretty . fst <$> positionalPs
    (ps', pTys', args') = if isNil namedPs
      then (
        hsep psNames,
        positionalPs',
        list psNames <++> parens (pretty "the (Parameters {tag = Void} [])" <++> list [])
      ) else (
        hsep psNames <++> pretty "ps",
        positionalPs' ++ [ret' <+> dot <+> pretty namedParameterNS <+> dot <+> parameters'],
        list psNames <++> pretty "ps"
      )
    fun = vcat [
      inline,
      pubExport,
      pretty funName <++> colon <++> pretty "HasIO io =>" <++> funType (pTys' ++ [pretty "io" <++> ret']),
      pretty funName <++> ps' <++> equals <++> pretty "primIO $ prim__dart_new" <++> ret' <++> args'
    ]
  in
    if isNil namedPs
      then [fun]
      else [namedParameterDeclarationsFor c.name namedParameterNS namedPs, fun]

isIdrisKeyword : String -> Bool
isIdrisKeyword s = case s of
  "of" => True
  _ => False

functionPrimsFor : {auto c : Class} -> Function -> String -> Bool -> List (Doc ())
functionPrimsFor (Fun n ps ret) foreignName hasThis =
  let
    ret' = prettyType ret
    ps' = prettyParameter <$> ps
    psNames = parameterNames ps
    primName = pretty ("prim__" ++ n)
    primRet = pretty "PrimIO" <++> ret'
    primParamTys = ps' ++ [primRet]
    proxyParamTys = [pretty "io" <++> ret']
    thisTy = the (Lazy (Doc())) (parens (this <++> colon <++> pretty c.name))
    proxyName = pretty (if isIdrisKeyword n then n ++ "_" else n)
    prim =
      foreign foreignName <+> hardline
        <+> primName <++> colon <++> funType (ifHasThis (thisTy :: primParamTys) primParamTys)
    proxy =
      export_ <+> proxyName <++> colon <++> pretty "HasIO io =>"
        <++> funType (ps' ++ ifHasThis (thisTy :: proxyParamTys) proxyParamTys) <+> hardline
        <+> proxyName <++> hsep psNames <++> ifHasThis (this <++> equals) equals
        <++> pretty "primIO $" <++> primName <++> hsep (ifHasThis (this :: psNames) psNames)
  in
    [prim, proxy]
  where
    ifHasThis : Lazy a -> Lazy a -> a
    ifHasThis ifTrue ifFalse = if hasThis then ifTrue else ifFalse

constPrim : {auto lib : Lib} -> String -> String -> DartType -> Doc ()
constPrim owner field ty =
  export_ <+> foreign ("const " ++ owner ++ "." ++ field ++ "," ++ lib.package) <+> hardline
    <+> pretty field <++> colon <++> prettyType ty

enumPrim : {auto lib : Lib} -> String -> String -> Doc ()
enumPrim n f = constPrim n f (NamedType n)

isAssignableFromInstance : String -> String -> Doc ()
isAssignableFromInstance super sub =
  export_ <+> pretty "IsAssignableFrom" <++> pretty super <++> pretty sub <++> pretty "where"

classMemberPrim : {auto lib : Lib} -> Class -> Member -> List (Doc ())
classMemberPrim c m = case m of
  Constructor n ps => ctorPrimsFor n ps
  Method f@(Fun n _ _) => functionPrimsFor f ("." ++ n) True
  Static f@(Fun n _ _) => functionPrimsFor f (foreignNameOf (c.name ++ "." ++ n)) False
  FieldMember (Var n ty) => [getter c.name n ty, setter c.name n ty]
  FieldMember (Final n ty) => [getter c.name n ty]
  Extends ty => [isAssignableFromInstance ty c.name]
  Const n ty => [constPrim c.name n ty]

prettyPrims : {auto lib : Lib} -> Declaration -> Maybe (Doc ())
prettyPrims (ClassDecl c) =
  case concatMap (classMemberPrim c) c.members of
    [] => Nothing
    ms => Just $ namespaced c.name (sepByLines ms)
prettyPrims (EnumDecl n fs) =
  Just $ namespaced n (vcat (enumPrim n <$> fs))

primsForLib : LibDef -> List (Doc ())
primsForLib libDef =
  let lib = libDef.lib
  in mapMaybe prettyPrims libDef.declarations

header : Doc () -> Doc ()
header n =
  pretty "||| FFI definitions for the " <++> n <++> "API." <+> hardline
    <+> pretty "module" <++> n <+> hardline <+> hardline
    <+> pretty "import Dart.Core" <+> hardline
    <+> pretty "import Dart.FFI"

export
renderModule : Module -> IO ()
renderModule ffi = do
  let decls = concatMap typeDeclsForLib ffi.libs
  let mutualDecls = pretty "mutual" <+> indented (sepByLines decls)
  let prims = concatMap primsForLib ffi.libs
  let doc = the (Doc ()) (sepByLines (header (pretty ffi.name) :: mutualDecls :: prims))
  let pageWidth = AvailablePerLine 120 1
  let options = record { layoutPageWidth = pageWidth } defaultLayoutOptions
  renderIO $ layoutPretty options $ doc
