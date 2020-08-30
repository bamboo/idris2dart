module Main

import Compiler.Common
import Compiler.ES.Imperative
import Core.Context
import Data.StringMap
import FFI
import Idris.Driver
import Printer
import System
import System.File
import Utils.Hex

data Dart : Type where

record DartT where
  constructor MkDartT
  imports : StringMap Doc

Statement : Type
Statement = ImperativeStatement

Expression : Type
Expression = ImperativeExp

dartIdent : String -> String
dartIdent s = concatMap okChar (unpack s)
  where
    okChar : Char -> String
    okChar c =
      if isAlphaNum c
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
  NS ns n => showSep "_" (reverse ns) ++ "_" ++ dartNameString n
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
assertionError e = "throw AssertionError" <+> paren e <+> semi

unsupportedError : Show a => a -> Doc
unsupportedError e = "throw UnsupportedError" <+> paren (debug e) <+> semi

unsupported : Show a => a -> Doc
unsupported e = "(() {" <+> unsupportedError e <+> "})()"

null' : Doc
null' = text "null"

dynamic' : Doc
dynamic' = text "dynamic "

final' : Doc
final' = text "final "

var' : Doc
var' = text "var "

paramList : List Name -> Doc
paramList ps = tupled ((dynamic' <+>) . dartName <$> ps)

castTo : Doc -> Doc -> Doc
castTo ty e = paren (e <+> " as " <+> ty)

dartConstant : Constant -> Doc
dartConstant c = case c of
  B8 i => shown i
  B16 i => shown i
  B32 i => shown i
  B64 i => shown i
  I i => shown i
  BI 0 => text "BigInt.zero"
  BI 1 => text "BigInt.one"
  BI i => "BigInt.from(" <+> shown i <+> ")"
  Ch c => shown (cast {to=Int} c)
  Str s => dartStringDoc s
  Db d => shown d
  WorldVal => text "#IdrisWorld"
  _ => unsupported c

runtimeTypeOf : Constant -> Doc
runtimeTypeOf ty = case ty of
  StringType => "String"
  IntegerType => "BigInt"
  DoubleType => "double"
  _ => "int"

dartForeign : {auto ctx : Ref Dart DartT} -> Name -> ForeignDartSpec -> Core Doc
dartForeign n (ForeignDartName lib f) = do
  alias <- addImport lib
  pure (final' <+> dartName n <+> " = " <+> alias <+> dot <+> text f <+> semi)

foreignDecl : {auto ctx : Ref Dart DartT} -> Name -> List String -> Core Doc
foreignDecl n ss = case n of
  NS _ (UN "prim__putStr") => do
    alias <- addImport "dart:io"
    pure (dartName n <+> "(s, w)" <+> block (alias <+> ".stdout.write(s);" <+> line <+> "return w;"))
  _ => case mapMaybe foreignDartSpecFrom ss of
    [s] => dartForeign n s
    _ => pure (dartName n <+> "([a, b, c, d, e])" <+> block (unsupportedError ss))

binOp : Doc -> Doc -> Doc -> Doc
binOp o lhs rhs = paren (lhs <+> o <+> rhs)

binOpOf' : Doc -> Doc -> Doc -> Doc -> Doc
binOpOf' ty op lhs rhs = binOp op (castTo ty lhs) (castTo ty rhs)

binOpOf : Constant -> Doc -> Doc -> Doc -> Doc
binOpOf ty = binOpOf' (runtimeTypeOf ty)

boolOpOf : Constant -> Doc -> Doc -> Doc -> Doc
boolOpOf ty o lhs rhs = paren (binOpOf ty o lhs rhs <+> " ? 1 : 0")

stringOp : Doc -> Doc -> Doc
stringOp s m = (castTo "String" s) <+> dot <+> m

bigIntToInt : Doc -> Doc
bigIntToInt i = castTo "BigInt" i <+> ".toInt()"

dartOp : {auto ctx : Ref Dart DartT}
  -> PrimFn arity
  -> Vect arity Doc
  -> Doc
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
dartOp (Neg ty) [x] = "(-" <+> castTo (runtimeTypeOf ty) x <+> ")"
dartOp StrLength [x] = stringOp x "length"
dartOp StrHead [x] = stringOp x "codeUnitAt(0)"
dartOp StrIndex [x, y] = stringOp x "codeUnitAt" <+> paren y
dartOp StrTail [x] = stringOp x "substring(1)"
dartOp StrCons [x, y] = binOp "+" ("String.fromCharCode" <+> paren x) (castTo "String" y)
dartOp StrAppend [x, y] = binOpOf' "String" "+" x y
dartOp (Cast ty StringType) [x] = x <+> ".toString()"
dartOp (Cast ty IntegerType) [x] = "BigInt.from" <+> paren x
dartOp (Cast IntegerType IntType) [x] = bigIntToInt x
dartOp (Cast CharType IntType) [x] = x
dartOp e args = unsupported (e, args)

mutual
  dartLambda : {auto ctx : Ref Dart DartT} -> List Name -> Statement -> Core Doc
  dartLambda ps s = pure (paramList ps <+> block !(dartStatement s))

  {-
    TODO:
    data ImperativeExp =
                      | IEPrimFnExt Name (List ImperativeExp)
                      | IEDelay ImperativeExp
                      | IEForce ImperativeExp
  -}
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
    IEConstructor (Left tag) [] =>
      pure (text ("const [" ++ show tag ++ "]"))
    IEConstructor (Left tag) args => do
      args' <- traverse dartExp args
      pure (text ("[" ++ show tag ++ ", ") <+> commaSep args' <+> "]")
    IEConstructorHead e => do
      e' <- dartExp e
      pure (castTo "int" (castTo "List" e' <+> "[0]"))
    IEConstructorTag (Left tag) =>
      pure (shown tag)
    IEConstructorArg i e => do
      e' <- dartExp e
      pure (castTo "List" e' <+> "[" <+> shown i <+> "]")
    _ => pure (debug e)

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
      pure (dartName n <+> !(dartLambda ps body))
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
    ForeignDecl n ss =>
      foreignDecl n ss
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

compileToDart : Ref Ctxt Defs -> ClosedTerm -> Core Doc
compileToDart defs term = do
  (impDefs, impMain) <- compileToImperative defs term
  ctx <- newRef Dart (MkDartT empty)
  dartDefs <- dartStatement impDefs
  dartMain <- dartStatement impMain
  let imports' = dartImport <$> toList !(imports <$> get Dart)
  let header = vcat imports'
  let mainDecl = "void main() {" <+> indented dartMain <+> "}"
  pure (header <+> line <+> mainDecl <+> line <+> dartDefs <+> line)

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
