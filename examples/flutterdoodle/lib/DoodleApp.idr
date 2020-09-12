||| FFI definitions for file:///./DoodleApp.dart
module DoodleApp

import public Flutter

%foreign "Dart:runDoodleApp,./DoodleApp.dart"
prim__runDoodleApp :
  (initialState : AnyPtr) ->
  (onTapDown : TapDownDetails -> AnyPtr -> PrimIO AnyPtr) ->
  (onLongPressMoveUpdate : LongPressMoveUpdateDetails -> AnyPtr -> PrimIO AnyPtr) ->
  (onPaint : Canvas -> Size -> AnyPtr -> PrimIO ()) ->
  PrimIO ()

export
runDoodleApp : HasIO io =>
  s ->
  (TapDownDetails -> s -> IO s) ->
  (LongPressMoveUpdateDetails -> s -> IO s) ->
  (Canvas -> Size -> s -> IO ()) ->
  io ()
runDoodleApp initialState onTapDown onLongPressMoveUpdate onPaint = primIO $
  prim__runDoodleApp
    (believe_me initialState) -- Let Idris believe `s` values can be passed around as `AnyPtr`
    (\d, s => toPrim $ believe_me $ onTapDown d (believe_me s))
    (\d, s => toPrim $ believe_me $ onLongPressMoveUpdate d (believe_me s))
    (\c, sz, s => toPrim $ believe_me $ onPaint c sz (believe_me s))