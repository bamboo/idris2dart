module SimpleFFI.Renderer

import SimpleFFI.Spec
import Data.List
import Data.SortedMap
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.String

emptyLine : Doc ()
emptyLine = hardline <+> hardline

sepByLines : List (Doc ()) -> Doc ()
sepByLines ds@(d::_) = foldl1 (\acc, d => acc <+> emptyLine <+> d) ds
sepByLines [] = emptyDoc

pubExport : Doc ()
pubExport = pretty "public export" <+> hardline

export_ : Doc ()
export_ = pretty "export" <+> hardline

typeHeader : Doc () -> Doc ()
typeHeader n =
  pubExport <+> n <++> colon <++> pretty "Type"

arr : Doc ()
arr = pretty " ->"

funType : List (Doc ()) -> Doc ()
funType = hsep . punctuate arr

prettyType : DartType -> Doc ()
prettyType ty = case ty of
  IntType => pretty "Int"
  StringType => pretty "String"
  DoubleType => pretty "Double"
  VoidType => pretty "()"
  NamedType n => pretty n
  FunctionType ps ret =>
    let sig = ps ++ [ret]
    in parens (funType (prettyType <$> sig))

structField : (String, DartType) -> Doc ()
structField (n, ty) =
  parens $ hsep $ punctuate comma $ [dquotes (pretty n), prettyType ty]

indented : Doc () -> Doc ()
indented d = nest 2 (hardline <+> d) <+> hardline

fieldList : List (String, DartType) -> Doc ()
fieldList [] = pretty "[]"
fieldList fs =
  enclose "[" "]"
    (indented . vsep . punctuate comma $ (structField <$> fs))

typeDecl : {auto lib : Lib} -> String -> List (String, DartType) -> Doc ()
typeDecl name fields =
  let
    name' = pretty name
    foreignTypeName = dquotes (name' <+> comma <+> pretty lib.package)
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
  typeDecl c.name (nameAndType <$> c.fields)
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
      <++> this <+> dot <+> pretty "setField" <++> dquotes field' <++> val

getter : String -> String -> DartType -> Doc ()
getter owner field ty =
  let
    field' = pretty field
    ty' = prettyType ty
  in
    export_
      <+> field' <++> colon <++> funType [pretty owner, ty'] <+> hardline
      <+> field' <++> this <++> equals
      <++> this <+> dot <+> pretty "getField" <++> dquotes field'

foreign : Doc ()
foreign = pretty "%foreign"

prettyParameter : Parameter -> Doc ()
prettyParameter (P n ty) = parens (pretty n <++> colon <++> prettyType ty)

parameterNames : List Parameter -> List (Doc ())
parameterNames ps = (\(P n _) => pretty n) <$> ps

methodPrimsFor : {auto c : Class} -> Function -> List (Doc ())
methodPrimsFor (Fun n ps ret) =
  let
    thisTy = parens (this <++> colon <++> pretty c.name)
    ret' = prettyType ret
    ps' = prettyParameter <$> ps
    psNames = ps.parameterNames
    primName = pretty ("prim__" ++ n)
    prim =
      foreign <++> dquotes (pretty ("Dart:." ++ n)) <+> hardline
        <+> primName <++> colon <++> funType (thisTy :: ps' ++ [pretty "PrimIO" <++> ret'])
    method =
      export_ <+> pretty n <++> colon <++> pretty "HasIO io =>" <++> funType (ps' ++ [thisTy, pretty "io" <++> ret']) <+> hardline
        <+> pretty n <++> hsep psNames <++> this <++> equals
        <++> pretty "primIO $" <++> this <+> dot <+> primName <++> hsep psNames
  in
    [prim, method]

ctorPrimsFor : {auto lib : Lib} -> {auto c : Class} -> String -> List Parameter -> List (Doc ())
ctorPrimsFor n ps =
  let
    ret' = pretty c.name
    ps' = prettyParameter <$> ps
    psNames = ps.parameterNames
    (idrisSuffix, dartSuffix, funName) =
      if length n == 0
        then ("", "", "new")
        else ("_" ++ n, "." ++ n, n)
    primName = pretty "prim__" <+> ret' <+> pretty idrisSuffix
    prim =
      foreign <++> dquotes (pretty ("Dart:" ++ c.name ++ dartSuffix ++ "," ++ lib.package)) <+> hardline
        <+> primName <++> colon <++> funType (ps' ++ [pretty "PrimIO" <++> ret'])
    fun =
      export_ <+> pretty funName <++> colon <++> pretty "HasIO io =>" <++> funType (ps' ++ [pretty "io" <++> ret']) <+> hardline
        <+> pretty funName <++> hsep psNames <++> equals
        <++> pretty "primIO $" <++> primName <++> hsep psNames
  in
    [prim, fun]

classMemberPrim : {auto lib : Lib} -> Class -> Member -> List (Doc ())
classMemberPrim c m = case m of
  Method f => methodPrimsFor f
  Constructor n ps => ctorPrimsFor n ps
  FieldMember (Var n ty) => [getter c.name n ty, setter c.name n ty]
  FieldMember (Final n ty) => [getter c.name n ty]

enumPrim : {auto lib : Lib} -> String -> String -> Doc ()
enumPrim n f =
  export_ <+> foreign <++> dquotes (pretty ("Dart:const " ++ n ++ "." ++ f ++ "," ++ lib.package)) <+> hardline
    <+> pretty f <++> colon <++> pretty n

namespaced : String -> Doc () -> Doc ()
namespaced n body = pretty ("namespace " ++ n) <+> indented body

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
    <+> pretty "import System.FFI"

export
renderModule : Module -> IO ()
renderModule ffi = do
  let decls = concatMap typeDeclsForLib ffi.libs
  let mutualDecls = pretty "mutual" <+> indented decls.sepByLines
  let prims = concatMap primsForLib ffi.libs
  let doc = the (Doc ()) (sepByLines (header (pretty ffi.name) :: mutualDecls :: prims))
  let pageWidth = AvailablePerLine 120 1
  let options = record { layoutPageWidth = pageWidth } defaultLayoutOptions
  renderIO $ layoutPretty options $ doc
