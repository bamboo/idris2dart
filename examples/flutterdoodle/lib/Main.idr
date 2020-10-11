module Main

import DoodleApp

infixr 1 //

||| In search of a convenient "OO style" method invocation syntax.
||| `obj // method arg1 arg2`
(//) : a -> (a -> b) -> b
(//) a f = f a

data State
  = Idle (List Offset)
  | Pressing Offset (List Offset)

onTapUp : TapUpDetails -> State -> IO State
onTapUp d s = case s of
  Idle os => pure (Idle (localPosition d :: os))
  s => pure s

onLongPressStart : LongPressStartDetails -> State -> IO State
onLongPressStart d s = case s of
  Idle os => pure (Pressing (localPosition d) os)
  s => pure s

onLongPressMoveUpdate : LongPressMoveUpdateDetails -> State -> IO State
onLongPressMoveUpdate d s = case s of
  Pressing _ os => pure (Pressing (localPosition d) os)
  s => pure s

onLongPressEnd : LongPressEndDetails -> State -> IO State
onLongPressEnd d s = case s of
  Pressing _ os => pure (Idle (localPosition d :: os))
  s => pure s

drawLines : Canvas -> List Offset -> IO ()
drawLines _ [] = pure ()
drawLines c (o :: os) = do
  path <- Path.new
  path // moveTo (dx o) (dy o)
  traverse (\o => path // lineTo (dx o) (dy o)) os
  paint <- Paint.new
  paint // setStyle PaintingStyle.stroke
  c // drawPath path paint

onPaint : Canvas -> Size -> State -> IO ()
onPaint c _ s = case s of
  Idle os => drawLines c os
  Pressing o os => drawLines c (o :: os)

main : IO ()
main = runDoodleApp (Idle []) onTapUp onLongPressStart onLongPressMoveUpdate onLongPressEnd onPaint
