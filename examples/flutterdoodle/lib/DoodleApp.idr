||| FFI definitions for file:///./DoodleApp.dart
module DoodleApp

import public Flutter
import public System.FFI

%foreign "Dart:runDoodleApp,./DoodleApp.dart"
prim__runDoodleApp :
  (initialState : AnyPtr) ->
  (onTapUp : TapUpDetails -> AnyPtr -> PrimIO AnyPtr) ->
  (onLongPressStart : LongPressStartDetails -> AnyPtr -> PrimIO AnyPtr) ->
  (onLongPressMoveUpdate : LongPressMoveUpdateDetails -> AnyPtr -> PrimIO AnyPtr) ->
  (onLongPressEnd : LongPressEndDetails -> AnyPtr -> PrimIO AnyPtr) ->
  (onPaint : Canvas -> Size -> AnyPtr -> PrimIO ()) ->
  PrimIO ()

export
runDoodleApp : HasIO io =>
  s ->
  (TapUpDetails -> s -> IO s) ->
  (LongPressStartDetails -> s -> IO s) ->
  (LongPressMoveUpdateDetails -> s -> IO s) ->
  (LongPressEndDetails -> s -> IO s) ->
  (Canvas -> Size -> s -> IO ()) ->
  io ()
runDoodleApp initialState onTapUp onLongPressStart onLongPressMoveUpdate onLongPressEnd onPaint = primIO $
  prim__runDoodleApp
    (believe_me initialState) -- Let Idris believe `s` values can be passed around as `AnyPtr`
    (\d, s => toPrim $ believe_me $ onTapUp d (believe_me s))
    (\d, s => toPrim $ believe_me $ onLongPressStart d (believe_me s))
    (\d, s => toPrim $ believe_me $ onLongPressMoveUpdate d (believe_me s))
    (\d, s => toPrim $ believe_me $ onLongPressEnd d (believe_me s))
    (\c, sz, s => toPrim $ believe_me $ onPaint c sz (believe_me s))