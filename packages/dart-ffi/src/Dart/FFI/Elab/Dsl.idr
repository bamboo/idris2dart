module Dart.FFI.Elab.Dsl

import public Dart.FFI.Elab.Types

public export
package : String -> List DartDecl -> DartPackage
package = MkDartPackage

public export
class' : DartName -> List DartDecl -> DartDecl
class' = Class

public export
generic : List DartName -> DartDecl -> DartDecl
generic = Generic

public export
extends : DartType -> DartDecl
extends = Extends

public export
enum : DartName -> List DartName -> DartDecl
enum = Enum

public export
fun : DartType -> DartName -> List DartParameter -> DartDecl
fun = Function

public export
new : DartName -> List DartParameter -> DartDecl
new = Constructor

public export
final : DartType -> DartName -> DartDecl
final = Val

public export
var : DartType -> DartName -> DartDecl
var = Var

public export
static : DartDecl -> DartDecl
static = Static

public export
io : DartType -> DartName -> List DartParameter -> DartDecl
io ty name ps = Effectful (fun ty name ps)
