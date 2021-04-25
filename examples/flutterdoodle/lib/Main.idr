module Main

import Flutter

%hide Dart.Core.List

data State
  = Idle (List Offset)
  | Pressing Offset (List Offset)

onTapUp : TapUpDetails -> State -> State
onTapUp d s = case s of
  Idle os => Idle (localPosition d :: os)
  s => s

onLongPressStart : LongPressStartDetails -> State -> State
onLongPressStart d s = case s of
  Idle os => Pressing (localPosition d) os
  s => s

onLongPressMoveUpdate : LongPressMoveUpdateDetails -> State -> State
onLongPressMoveUpdate d s = case s of
  Pressing _ os => Pressing (localPosition d) os
  s => s

onLongPressEnd : LongPressEndDetails -> State -> State
onLongPressEnd d s = case s of
  Pressing _ os => Idle (localPosition d :: os)
  s => s

drawLines : Canvas -> List Offset -> IO ()
drawLines _ [] = pure ()
drawLines c (o :: os) = do
  path <- Path.new
  path @. moveTo (dx o) (dy o)
  for_ os \o => path @. lineTo (dx o) (dy o)
  paint <- Paint.new
  paint @. setStyle PaintingStyle.stroke
  c @. drawPath path paint

onPaint : State -> Canvas -> Size -> IO ()
onPaint s c _ = case s of
  Idle os => drawLines c os
  Pressing o os => drawLines c (o :: os)

appTitle : String
appTitle = "Try taps and long presses"

appHome : Stateful
appHome = Stateful.new [initialState @= Idle [], onBuild @= build]
  where
    build : StatefulWidgetState State -> BuildContext -> IO Widget
    build state context = widget $ Scaffold.new [
      appBar @=> !(AppBar.new [
        title @=> Text.new appTitle []
      ]),
      body @=> Center.new [
        child @=> CustomPaint.new [
          child @=> !(GestureDetector.new [
            onTapUp @= modify state . onTapUp,
            onLongPressStart @= modify state . onLongPressStart,
            onLongPressMoveUpdate @= modify state . onLongPressMoveUpdate,
            onLongPressEnd @= modify state . onLongPressEnd
          ]),
          painter @=> Painter.new [
            onPaint @= onPaint (get state)
          ]
        ]
      ]
    ]

app : IO MaterialApp
app = pure $ MaterialApp.new [
  title @= appTitle,
  theme @= !(ThemeData.new [
    primarySwatch @= Colors.blue,
    visualDensity @= VisualDensity.adaptivePlatformDensity
  ]),
  home @=> appHome
]

main : IO ()
main = runApp !app
