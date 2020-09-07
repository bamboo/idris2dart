module Main

import Flutter

infixr 1 //

||| In search of a convenient "OO style" method invocation syntax.
||| `obj // method arg1 arg2`
(//) : a -> (a -> b) -> b
(//) a f = f a

Point : Type
Point = (Double, Double)

State : Type
State = List Point

onTapDown : Double -> Double -> State -> IO State
onTapDown x y s = pure ((x, y) :: s)

onPaint : Canvas -> Double -> Double -> State -> IO ()
onPaint c w h [] = pure ()
onPaint c w h ((x0, y0) :: ps) = do
  path <- Path.new
  path // moveTo x0 y0
  traverse (\(x, y) => path // lineTo x y) ps
  paint <- Paint.new
  paint // setStyle PaintingStyle.stroke
  c // drawPath path paint

%foreign "Dart:runDoodleApp,./view.dart"
prim__runDoodleApp :
     (initialState : State)
  -> (onTapDown : Double -> Double -> State -> PrimIO State)
  -> (onPaint : Canvas -> Double -> Double -> State -> PrimIO ())
  -> PrimIO ()

runDoodleApp : HasIO io
  => State
  -> (Double -> Double -> State -> IO State)
  -> (Canvas -> Double -> Double -> State -> IO ())
  -> io ()
runDoodleApp initialState onTapDown onPaint = primIO $
  prim__runDoodleApp
    initialState
    (\x, y, s => toPrim $ onTapDown x y s)
    (\c, w, h, s => toPrim $ onPaint c w h s)

main : IO ()
main = runDoodleApp [] onTapDown onPaint
