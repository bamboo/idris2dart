module Main

import Compiler.Common
import Compiler.ES.Imperative
import Core.CompileExpr
import Core.Context
import Data.List
import Data.String.Extra
import Data.StringMap
import Data.Strings
import Idris.Driver
import Printer
import System
import System.File
import Utils.Hex

data Dart : Type where

record DartT where
  constructor MkDartT
  imports : StringMap Doc
  usesDelay : Bool

Statement : Type
Statement = ImperativeStatement

Expression : Type
Expression = ImperativeExp

dartIdent : String -> String
dartIdent s = concatMap okChar (unpack s)
  where
    okChar : Char -> String
    okChar c =
      if isAlphaNum c || c == '_'
        then cast c
        else "$" ++ asHex (cast c)

addImport : {auto ctx : Ref Dart DartT} -> String -> Core Doc
addImport lib = do
  s <- get Dart
  case lookup lib (imports s) of
    Nothing => do
      let alias = text (dartIdent lib)
      put Dart (record { imports $= insert lib alias } s)
      pure alias
    Just alias =>
      pure alias

keywordSafe : String -> String
keywordSafe s = case s of
  "var" => "var_"
  _ => s

dartNameString : Name -> String
dartNameString n = case n of
  NS ns n => showNSWithSep "_" ns ++ "_" ++ dartNameString n
  UN n => keywordSafe (dartIdent n)
  MN n i => dartIdent n ++ "_" ++ show i
  PV n d => "pat__" ++ dartNameString n
  DN _ n => dartNameString n
  Nested (i, x) n => "n__" ++ show i ++ "_" ++ show x ++ "_" ++ dartNameString n
  CaseBlock x y => "case__" ++ dartIdent x ++ "_" ++ show y
  WithBlock x y => "with__" ++ dartIdent x ++ "_" ++ show y
  Resolved i => "fn__" ++ show i

dartName : Name -> Doc
dartName = text . dartNameString

dartString : String -> String
dartString s = "\"" ++ (concatMap okChar (unpack s)) ++ "\""
  where
    okChar : Char -> String
    okChar c = case c of
      '\0' => "\\0"
      '"' => "\\\""
      '\r' => "\\r"
      '\n' => "\\n"
      '$' => "\\$"
      '\\' => "\\\\"
      c => if (c >= ' ') && (c <= '~')
        then cast c
        else "\\u{" ++ asHex (cast c) ++ "}"

dartStringDoc : String -> Doc
dartStringDoc = text . dartString

debug : Show e => e -> Doc
debug = dartStringDoc . show

assertionError : Doc -> Doc
assertionError e = "throw $.AssertionError" <+> paren e <+> semi

unsupportedError : Show a => a -> Doc
unsupportedError e = "throw $.UnsupportedError" <+> paren (debug e) <+> semi

unsupported : Show a => a -> Doc
unsupported e = "(() {" <+> unsupportedError e <+> "})()"

null' : Doc
null' = text "null"

dynamic' : Doc
dynamic' = text "$.dynamic "

final' : Doc
final' = text "final "

var' : Doc
var' = text "var "

paramList : List Name -> Doc
paramList ps = tupled ((dynamic' <+>) . dartName <$> ps)

castTo : Doc -> Doc -> Doc
castTo ty e = paren (e <+> " as " <+> ty)

world' : Doc
world' = text "#World"

intTy : Doc
intTy = text "$.int"

bigIntTy : Doc
bigIntTy = text "$.BigInt"

stringTy : Doc
stringTy = text "$.String"

doubleTy : Doc
doubleTy = text "$.double"

maxDartInt : Integer
maxDartInt = 9223372036854775807

dartConstant : Constant -> Doc
dartConstant c = case c of
  B8 i => shown i
  B16 i => shown i
  B32 i => shown i
  B64 i => shown i
  I i => shown i
  BI 0 => text "$.BigInt.zero"
  BI 1 => text "$.BigInt.one"
  BI i =>
    if i > maxDartInt
      then "$.BigInt.parse(\"" <+> shown i <+> "\")"
      else "$.BigInt.from(" <+> shown i <+> ")"
  Ch c => shown (cast {to=Int} c)
  Str s => dartStringDoc s
  Db d => shown d
  WorldVal => world'
  _ => unsupported c

runtimeTypeOf : Constant -> Doc
runtimeTypeOf ty = case ty of
  StringType => stringTy
  IntegerType => bigIntTy
  DoubleType => doubleTy
  _ => "$.int"

splitAtFirst : Char -> String -> (String, String)
splitAtFirst ch s =
  let (before, rest) = break (== ch) s
  in (before, drop 1 rest)

---- Dart FFI -------------------------------------
Lib : Type
Lib = String

data ForeignDartSpec
  = ForeignFunction String Lib
  | ForeignConst String Lib
  | ForeignMethod String

dropPrefix : String -> String -> Maybe String
dropPrefix p s =
  if p `isPrefixOf` s
    then Just (drop (length p) s)
    else Nothing

foreignDartSpecFrom : String -> Maybe ForeignDartSpec
foreignDartSpecFrom s = do
  (name, lib) <- dartNameAndLib s
  method name <|> const name lib <|> function name lib
  where
    dartNameAndLib : String -> Maybe (String, Lib)
    dartNameAndLib s = splitAtFirst ',' <$> dropPrefix "Dart:" s

    method : String -> Maybe ForeignDartSpec
    method m = ForeignMethod <$> dropPrefix "." m

    const : String -> Lib -> Maybe ForeignDartSpec
    const n lib = (`ForeignConst` lib) <$> dropPrefix "const " n

    function : String -> Lib -> Maybe ForeignDartSpec
    function n lib = Just (ForeignFunction n lib)

uncurriedSignature : CFType -> CFType -> (List CFType, CFType)
uncurriedSignature a b = go [a] b
  where
    go : List CFType -> CFType -> (List CFType, CFType)
    go ps (CFFun a b) = go (a :: ps) b
    go ps ret = (reverse ps, ret)

argNamesFor : List a -> String -> List Doc
argNamesFor args p = text . (p ++) . show <$> [1..length args]

||| Adapts the foreign callback [c] so that it can be
||| invoked from Dart with the expected signature.
foreignCallback : (c : Doc) -> CFType -> CFType -> Doc
foreignCallback c a b =
  let
    (args, ret) = uncurriedSignature a b
    argNames = argNamesFor args "c$"
    params = mapMaybe callbackParam (zip argNames args)
    worldArg = case ret of
      CFIORes _ => paren world'
      _ => empty
    callbackArgs = hcat (paren <$> params) <+> worldArg
  in
    tupled params <+> " => " <+> c <+> callbackArgs
  where
    callbackParam : (Doc, CFType) -> Maybe Doc
    callbackParam (p, ty) = case ty of
      CFWorld => Nothing
      _ => Just p

foreignArg : (Doc, CFType) -> Maybe Doc
foreignArg (n, ty) = case ty of
  CFWorld => Nothing
  CFFun a b => Just (foreignCallback n a b)
  CFUser (UN "Type") [] => Nothing
  CFUser (UN "__") [] => Nothing
  _ => Just n

singleExpFunction : Doc -> List Doc -> Doc -> Doc
singleExpFunction n ps e =
  n <+> tupled ps <+> block ("return " <+> e <+> semi)

foreignFunctionProxy : Doc -> Doc -> List CFType -> CFType -> Doc
foreignFunctionProxy n ff args ret =
  let
    argNames = argNamesFor args "a$"
    fArgs = mapMaybe foreignArg (zip argNames args)
    fc = ff <+> tupled fArgs
  in
    singleExpFunction n argNames fc

foreignName : {auto ctx : Ref Dart DartT}
  -> Lib -> String -> Core Doc
foreignName lib n =
  pure (!(addImport lib) <+> dot <+> text n)

foreignTypeName : {auto ctx : Ref Dart DartT}
  -> String -> Core Doc
foreignTypeName ty = do
  let (tyN, lib) = splitAtFirst ',' ty
  foreignName lib tyN

foreignTypeOf : {auto ctx : Ref Dart DartT} -> CFType -> Core Doc
foreignTypeOf e = case e of
  CFString => pure stringTy
  CFDouble => pure doubleTy
  CFChar => pure intTy
  CFInt => pure intTy
  CFUnsigned8 => pure intTy
  CFUnsigned16 => pure intTy
  CFUnsigned32 => pure intTy
  CFUnsigned64 => pure intTy
  CFStruct name _ => foreignTypeName name
  _ => pure "$.dynamic"

foreignMethodProxy : {auto ctx : Ref Dart DartT}
  -> Doc -> Doc -> List CFType -> CFType -> Core Doc
foreignMethodProxy n m args@(thisTy :: _) ret = do
  let argNames = argNamesFor args "a$"
  case mapMaybe foreignArg (zip argNames args) of
    fThis :: fArgs => do
      let mc = castTo !(foreignTypeOf thisTy) fThis <+> dot <+> m <+> tupled fArgs
      pure (singleExpFunction n argNames mc)
    _ => pure (unsupported (m, args))
foreignMethodProxy n m args ret =
  pure (unsupported (m, args))

dartForeign : {auto ctx : Ref Dart DartT}
  -> Name -> ForeignDartSpec
  -> List CFType -> CFType
  -> Core Doc
dartForeign n (ForeignFunction f lib) args ret = do
  ff <- foreignName lib f
  pure (foreignFunctionProxy (dartName n) ff args ret)
dartForeign n (ForeignConst c lib) _ _ = do
  fc <- foreignName lib c
  pure (singleExpFunction (dartName n) [] fc)
dartForeign n (ForeignMethod m) args ret = do
  foreignMethodProxy (dartName n) (text m) args ret

dartStdout : {auto ctx : Ref Dart DartT} -> Core Doc
dartStdout = foreignName "dart:io" "stdout"

dartStdin : {auto ctx : Ref Dart DartT} -> Core Doc
dartStdin = foreignName "dart:io" "stdin"

foreignDecl : {auto ctx : Ref Dart DartT}
  -> Name -> List String
  -> List CFType -> CFType
  -> Core Doc
foreignDecl n ss args ret = case n of
  NS _ (UN "prim__putStr") =>
    pure (dartName n <+> "(s, w)" <+> block (!dartStdout <+> ".write(s);" <+> line <+> "return w;"))
  NS _ (UN "prim__putChar") =>
    pure (dartName n <+> "(c, w)" <+> block (!dartStdout <+> ".writeCharCode(c);" <+> line <+> "return w;"))
  NS _ (UN "prim__getStr") =>
    pure (dartName n <+> "(w)" <+> block ("return " <+> !dartStdin <+> ".readLineSync() ?? \"\";"))
  _ => case mapMaybe foreignDartSpecFrom ss of
    [s] => dartForeign n s args ret
    _ => pure (dartName n <+> "([a, b, c, d, e, f, g, h, i, j, k])" <+> block (unsupportedError ss))

binOp : Doc -> Doc -> Doc -> Doc
binOp o lhs rhs = paren (lhs <+> o <+> rhs)

binOpOf' : Doc -> Doc -> Doc -> Doc -> Doc
binOpOf' ty op lhs rhs = binOp op (castTo ty lhs) (castTo ty rhs)

binOpOf : Constant -> Doc -> Doc -> Doc -> Doc
binOpOf ty = binOpOf' (runtimeTypeOf ty)

idrisBoolFrom : Doc -> Doc
idrisBoolFrom e = paren (e <+> " ? 1 : 0")

boolOpOf : Constant -> Doc -> Doc -> Doc -> Doc
boolOpOf ty o lhs rhs = idrisBoolFrom (binOpOf ty o lhs rhs)

stringOp : Doc -> Doc -> Doc
stringOp s m = (castTo stringTy s) <+> dot <+> m

doubleOp : Doc -> Doc -> Doc
doubleOp e m = (castTo doubleTy e) <+> dot <+> m

bigIntToInt : Doc -> Doc
bigIntToInt i = castTo bigIntTy i <+> ".toInt()"

stringCompare : Doc -> Doc -> Doc -> Doc
stringCompare zeroComparison lhs rhs =
  idrisBoolFrom (stringOp lhs "compareTo" <+> paren rhs <+> zeroComparison)

runtimeCastOf : Constant -> Doc -> Doc
runtimeCastOf ty e = castTo (runtimeTypeOf ty) e

dartOp : {auto ctx : Ref Dart DartT}
  -> PrimFn arity
  -> Vect arity Doc
  -> Doc
dartOp (LT StringType) [x, y] = stringCompare " < 0" x y
dartOp (LTE StringType) [x, y] = stringCompare " <= 0" x y
dartOp (EQ StringType) [x, y] = stringCompare " == 0" x y
dartOp (GTE StringType) [x, y] = stringCompare " >= 0" x y
dartOp (GT StringType) [x, y] = stringCompare " > 0" x y
dartOp (LT ty) [x, y] = boolOpOf ty "<" x y
dartOp (LTE ty) [x, y] = boolOpOf ty "<=" x y
dartOp (EQ ty) [x, y] = boolOpOf ty "==" x y
dartOp (GTE ty) [x, y] = boolOpOf ty ">=" x y
dartOp (GT ty) [x, y] = boolOpOf ty ">" x y
dartOp (Add ty) [x, y] = binOpOf ty "+" x y
dartOp (Sub ty) [x, y] = binOpOf ty "-" x y
dartOp (Mul ty) [x, y] = binOpOf ty "*" x y
dartOp (Div ty) [x, y] = binOpOf ty "/" x y
dartOp (Mod ty) [x, y] = binOpOf ty "%" x y
dartOp (Neg ty) [x] = paren ("-" <+> runtimeCastOf ty x)
dartOp StrLength [x] = stringOp x "length"
dartOp StrHead [x] = stringOp x "codeUnitAt(0)"
dartOp StrIndex [x, y] = stringOp x "codeUnitAt" <+> paren y
dartOp StrTail [x] = stringOp x "substring(1)"
dartOp StrCons [x, y] = binOp "+" ("$.String.fromCharCode" <+> paren x) (castTo stringTy y)
dartOp StrAppend [x, y] = binOpOf' stringTy "+" x y
dartOp (Cast StringType IntegerType) [x] = paren (bigIntTy <+> ".tryParse" <+> castTo stringTy x <+> " ?? " <+> bigIntTy <+> ".zero")
dartOp (Cast StringType DoubleType) [x] = paren (doubleTy <+> ".tryParse" <+> castTo stringTy x <+> " ?? 0.0")
dartOp (Cast StringType IntType) [x] = paren (intTy <+> ".tryParse" <+> castTo stringTy x <+> " ?? 0")
dartOp (Cast DoubleType IntType) [x] = doubleOp x "toInt()"
dartOp (Cast IntegerType IntType) [x] = bigIntToInt x
dartOp (Cast CharType IntType) [x] = x
dartOp (Cast IntType CharType) [x] = x
dartOp (Cast ty StringType) [x] = x <+> ".toString()"
dartOp (Cast ty IntegerType) [x] = "$.BigInt.from" <+> paren x
dartOp (Cast ty DoubleType) [x] = runtimeCastOf ty x <+> ".toDouble()"
dartOp DoubleFloor [x] = doubleOp x "floorToDouble()"
dartOp DoubleCeiling [x] = doubleOp x "ceilToDouble()"
dartOp BelieveMe [_, _, x] = x
dartOp e args = unsupported (e, args)

useDelay : {auto ctx : Ref Dart DartT} -> Core ()
useDelay = do
  s <- get Dart
  put Dart (record { usesDelay = True } s)

mutual
  dartLambda : {auto ctx : Ref Dart DartT} -> List Name -> Statement -> Core Doc
  dartLambda ps s = pure (paramList ps <+> block !(dartStatement s))

  dartExp : {auto ctx : Ref Dart DartT}
    -> Expression
    -> Core Doc
  dartExp e = case e of
    IENull => pure null'
    IEConstant c => pure (dartConstant c)
    IEVar v => pure (dartName v)
    IELambda ps s => dartLambda ps s
    IEApp f args => do
      f' <- dartExp f
      args' <- traverse dartExp args
      pure (f' <+> tupled args')
    IEPrimFn f args =>
      pure (dartOp f !(traverseVect dartExp args))
    IEPrimFnExt n args =>
      dartPrimFnExt n args
    IEConstructor (Left tag) [] =>
      pure (text ("const [" ++ show tag ++ "]"))
    IEConstructor (Left tag) args => do
      args' <- traverse dartExp args
      pure (text ("[" ++ show tag ++ ", ") <+> commaSep args' <+> "]")
    IEConstructorHead e => do
      e' <- dartExp e
      pure (castTo "$.int" (castTo "$.List" e' <+> "[0]"))
    IEConstructorTag (Left tag) =>
      pure (shown tag)
    IEConstructorArg i e => do
      e' <- dartExp e
      pure (castTo "$.List" e' <+> "[" <+> shown i <+> "]")
    IEDelay e => do
      useDelay
      pure ("$Delayed" <+> paren ("() => " <+> !(dartExp e)))
    IEForce e =>
      pure (castTo "$Delayed" !(dartExp e) <+> ".force()")
    _ => pure (debug e)

  dartPrimFnExt : {auto ctx : Ref Dart DartT}
    -> Name -> List Expression -> Core Doc
  dartPrimFnExt
    (NS _ (UN "prim__getField"))
    (IEConstant (Str ty) :: _ :: _ :: e :: IEConstant (Str f) :: _) = do
      fTy <- foreignTypeName ty
      pure (castTo fTy !(dartExp e) <+> dot <+> text f)
  dartPrimFnExt
    (NS _ (UN "prim__setField"))
    (IEConstant (Str ty) :: _ :: _ :: e :: IEConstant (Str f) :: _ :: rhs :: _) = do
      fTy <- foreignTypeName ty
      pure (castTo fTy !(dartExp e) <+> dot <+> text f <+> " = " <+> !(dartExp rhs))
  dartPrimFnExt n args = pure (debug (n, args))

  dartCase : {auto ctx : Ref Dart DartT}
    -> (Expression, Statement)
    -> Core Doc
  dartCase (e, s) = do
    e' <- case e of
      IEConstant (BI i) => pure (shown i)
      _ => dartExp e
    s' <- dartStatement s
    pure ("case " <+> e' <+> ":" <+> block s' <+> line <+> "break;")

  defaultDartCase : {auto ctx : Ref Dart DartT}
    -> Maybe Statement
    -> Core Doc
  defaultDartCase dc = case dc of
    Just s => do
      s' <- dartStatement s
      pure (line <+> "default:" <+> block s' <+> line <+> "break;")
    Nothing => pure empty

  dartSwitch : {auto ctx : Ref Dart DartT}
    -> Doc
    -> List (Expression, Statement)
    -> Maybe Statement
    -> Core Doc
  dartSwitch e cases def = do
    cases' <- traverse dartCase cases
    def' <- defaultDartCase def
    pure ("switch " <+> paren e <+> block (vcat cases' <+> def'))

  dartStatement : {auto ctx : Ref Dart DartT}
    -> Statement
    -> Core Doc
  dartStatement s = case s of
    FunDecl fc n ps body =>
      pure (line <+> dartName n <+> !(dartLambda ps body))
    ForeignDecl fc n ss args ret =>
      pure (line <+> !(foreignDecl n ss args ret))
    SwitchStatement e cases@((IEConstant (BI _), _) :: _) maybeDefault =>
      dartSwitch (bigIntToInt !(dartExp e)) cases maybeDefault
    SwitchStatement e cases maybeDefault =>
      dartSwitch !(dartExp e) cases maybeDefault
    SeqStatement a b =>
      pure (!(dartStatement a) <+> line <+> !(dartStatement b))
    ReturnStatement e =>
      pure ("return " <+> !(dartExp e) <+> semi)
    EvalExpStatement e =>
      pure (!(dartExp e) <+> semi)
    ConstDecl n e =>
      dartVar final' n (Just e)
    LetDecl n maybeE =>
      dartVar var' n maybeE
    MutateStatement n e =>
      pure (dartName n <+> " = " <+> !(dartExp e) <+> semi)
    ErrorStatement s =>
      pure (assertionError (dartStringDoc s))
    ForEverLoop s =>
      pure ("while (true)" <+> block !(dartStatement s))
    CommentStatement c =>
      pure ("/* " <+> text c <+> "*/")
    DoNothing => pure empty

  dartVar : {auto ctx : Ref Dart DartT}
    -> (keyword : Doc)
    -> Name
    -> Maybe Expression
    -> Core Doc
  dartVar kw n init = case init of
    Just e => pure (kw <+> dartName n <+> " = " <+> !(dartExp e) <+> semi)
    Nothing => pure (kw <+> dartName n <+> semi)

dartImport : (String, Doc) -> Doc
dartImport (lib, alias) =
  "import \"" <+> text lib <+> "\" as " <+> alias <+> semi

header : List Doc
header = [
  text "// DO NOT CHANGE THIS FILE!",
  text "// It has been generated by idris2dart.",
  text "// ignore_for_file: non_constant_identifier_names",
  text "// ignore_for_file: unnecessary_cast"
]

delayClass : Doc
delayClass = text "
class $Delayed {
  $.dynamic Function() e;
  $.dynamic value;
  $Delayed(this.e);
  $.dynamic force() {
    final e = this.e;
    if (e != null) {
      this.value = e();
      this.e = null;
    }
    return this.value;
  }
}
"

compileToDart : Ref Ctxt Defs -> ClosedTerm -> Core Doc
compileToDart defs term = do
  (impDefs, impMain) <- compileToImperative defs term
  ctx <- newRef Dart (MkDartT (fromList [("dart:core", "$")]) False)
  dartDefs <- dartStatement impDefs
  dartMain <- dartStatement impMain
  finalState <- get Dart
  let imports' = dartImport <$> finalState.imports.toList
  let header' = vcat (header ++ imports')
  let mainDecl = "void main()" <+> block dartMain
  let footer = if finalState.usesDelay then delayClass else empty
  pure (header' <+> emptyLine <+> mainDecl <+> line <+> dartDefs <+> line <+> delayClass)

compileToDartFile : String -> Ref Ctxt Defs -> ClosedTerm -> Core ()
compileToDartFile file defs term = do
  dartDoc <- compileToDart defs term
  Right _ <- coreLift (writeDocToFile file dartDoc)
    | Left err => throw (FileErr file err)
  pure ()

compile : Ref Ctxt Defs
  -> (tmpDir : String)
  -> (outputDir : String)
  -> ClosedTerm
  -> (outfile : String)
  -> Core (Maybe String)
compile defs tmpDir outputDir term file = do
  compileToDartFile file defs term
  pure Nothing

execute : Ref Ctxt Defs -> (tmpDir : String) -> ClosedTerm -> Core ()
execute defs tmpDir term = do
  let tempDartFile = tmpDir ++ "/temp.dart"
  compileToDartFile tempDartFile defs term
  coreLift (system ("dart" ++ " " ++ tempDartFile))
  pure ()

dartCodegen : Codegen
dartCodegen = MkCG compile execute

main : IO ()
main = mainWithCodegens [("dart", dartCodegen)]
