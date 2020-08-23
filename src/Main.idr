module Main

import Core.Context
import Compiler.Common
import Compiler.ES.Imperative
import Idris.Driver

compile : Ref Ctxt Defs -> (tmpDir : String) -> (outputDir : String) ->
        ClosedTerm -> (outfile : String) -> Core (Maybe String)
compile defs tmpDir outputDir term file = do
  (impDefs, impMain) <- compileToImperative defs term
  coreLift $ printLn impDefs
  coreLift $ printLn impMain
  pure $ Nothing

execute : Ref Ctxt Defs -> (tmpDir : String) -> ClosedTerm -> Core ()
execute defs tmpDir term = do
  (impDefs, impMain) <- compileToImperative defs term
  coreLift $ printLn impDefs
  coreLift $ printLn impMain

dartCodegen : Codegen
dartCodegen = MkCG compile execute

main : IO ()
main = mainWithCodegens [("dart", dartCodegen)]

{-
mutual
  public export
  data ImperativeExp = IEVar Name
                     | IELambda (List Name) ImperativeStatement
                     | IEApp ImperativeExp (List ImperativeExp)
                     | IEConstant Constant
                     | IEPrimFn (PrimFn arity) (Vect arity ImperativeExp)
                     | IEPrimFnExt Name (List ImperativeExp)
                     | IEConstructorHead ImperativeExp
                     | IEConstructorTag (Either Int String)
                     | IEConstructorArg Int ImperativeExp
                     | IEConstructor (Either Int String) (List ImperativeExp)
                     | IEDelay ImperativeExp
                     | IEForce ImperativeExp
                     | IENull

  public export
  data ImperativeStatement = DoNothing
                           | SeqStatement ImperativeStatement ImperativeStatement
                           | FunDecl FC Name (List Name) ImperativeStatement
                           | ForeignDecl Name (List String)
                           | ReturnStatement ImperativeExp
                           | SwitchStatement ImperativeExp (List (ImperativeExp, ImperativeStatement)) (Maybe ImperativeStatement)
                           | LetDecl Name (Maybe ImperativeExp)
                           | ConstDecl Name ImperativeExp
                           | MutateStatement Name ImperativeExp
                           | ErrorStatement String
                           | EvalExpStatement ImperativeExp
                           | CommentStatement String
                           | ForEverLoop ImperativeStatement
-}
