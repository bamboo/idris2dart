||| FFI definitions for the  Flutter API.
module Flutter

import System.FFI

mutual
  public export
  Canvas : Type
  Canvas = Struct "Canvas,dart:ui" []

  public export
  PaintingStyle : Type
  PaintingStyle = Struct "PaintingStyle,dart:ui" []

  public export
  Paint : Type
  Paint = Struct "Paint,dart:ui" [
    ("style", PaintingStyle),
    ("strokeWidth", Double)
  ]

  public export
  Offset : Type
  Offset = Struct "Offset,dart:ui" [
    ("dx", Double),
    ("dy", Double)
  ]

  public export
  Size : Type
  Size = Struct "Size,dart:ui" [
    ("width", Double),
    ("height", Double)
  ]

  public export
  Path : Type
  Path = Struct "Path,dart:ui" []

  public export
  Velocity : Type
  Velocity = Struct "Velocity,dart:ui" [
    ("pixelsPerSecond", Offset)
  ]

  public export
  TapDownDetails : Type
  TapDownDetails = Struct "TapDownDetails,package:flutter/gestures.dart" [
    ("globalPosition", Offset),
    ("localPosition", Offset)
  ]

  public export
  TapUpDetails : Type
  TapUpDetails = Struct "TapUpDetails,package:flutter/gestures.dart" [
    ("globalPosition", Offset),
    ("localPosition", Offset)
  ]

  public export
  LongPressStartDetails : Type
  LongPressStartDetails = Struct "LongPressStartDetails,package:flutter/gestures.dart" [
    ("globalPosition", Offset),
    ("localPosition", Offset)
  ]

  public export
  LongPressMoveUpdateDetails : Type
  LongPressMoveUpdateDetails = Struct "LongPressMoveUpdateDetails,package:flutter/gestures.dart" [
    ("localOffsetFromOrigin", Offset),
    ("localPosition", Offset)
  ]

  public export
  LongPressEndDetails : Type
  LongPressEndDetails = Struct "LongPressEndDetails,package:flutter/gestures.dart" [
    ("globalPosition", Offset),
    ("localPosition", Offset)
  ]


namespace Canvas
  %foreign "Dart:.drawPath"
  prim__drawPath : (this : Canvas) -> (path : Path) -> (paint : Paint) -> PrimIO ()

  export
  drawPath : HasIO io => (path : Path) -> (paint : Paint) -> (this : Canvas) -> io ()
  drawPath path paint this = primIO $ prim__drawPath this path paint


namespace PaintingStyle
  export
  %foreign "Dart:const PaintingStyle.stroke,dart:ui"
  stroke : PaintingStyle


namespace Paint
  %foreign "Dart:Paint,dart:ui"
  prim__Paint : PrimIO Paint

  export
  new : HasIO io => io Paint
  new  = primIO $ prim__Paint 

  export
  style : Paint -> PaintingStyle
  style this = getField this "style"

  export
  setStyle : PaintingStyle -> Paint -> IO ()
  setStyle value this = setField this "style" value

  export
  strokeWidth : Paint -> Double
  strokeWidth this = getField this "strokeWidth"

  export
  setStrokeWidth : Double -> Paint -> IO ()
  setStrokeWidth value this = setField this "strokeWidth" value


namespace Offset
  %foreign "Dart:Offset,dart:ui"
  prim__Offset : (dx : Double) -> (dy : Double) -> PrimIO Offset

  export
  new : HasIO io => (dx : Double) -> (dy : Double) -> io Offset
  new dx dy = primIO $ prim__Offset dx dy

  export
  dx : Offset -> Double
  dx this = getField this "dx"

  export
  dy : Offset -> Double
  dy this = getField this "dy"


namespace Size
  export
  width : Size -> Double
  width this = getField this "width"

  export
  height : Size -> Double
  height this = getField this "height"


namespace Path
  %foreign "Dart:Path,dart:ui"
  prim__Path : PrimIO Path

  export
  new : HasIO io => io Path
  new  = primIO $ prim__Path 

  %foreign "Dart:.moveTo"
  prim__moveTo : (this : Path) -> (x : Double) -> (y : Double) -> PrimIO ()

  export
  moveTo : HasIO io => (x : Double) -> (y : Double) -> (this : Path) -> io ()
  moveTo x y this = primIO $ prim__moveTo this x y

  %foreign "Dart:.lineTo"
  prim__lineTo : (this : Path) -> (x : Double) -> (y : Double) -> PrimIO ()

  export
  lineTo : HasIO io => (x : Double) -> (y : Double) -> (this : Path) -> io ()
  lineTo x y this = primIO $ prim__lineTo this x y


namespace Velocity
  export
  pixelsPerSecond : Velocity -> Offset
  pixelsPerSecond this = getField this "pixelsPerSecond"


namespace TapDownDetails
  export
  globalPosition : TapDownDetails -> Offset
  globalPosition this = getField this "globalPosition"

  export
  localPosition : TapDownDetails -> Offset
  localPosition this = getField this "localPosition"


namespace TapUpDetails
  export
  globalPosition : TapUpDetails -> Offset
  globalPosition this = getField this "globalPosition"

  export
  localPosition : TapUpDetails -> Offset
  localPosition this = getField this "localPosition"


namespace LongPressStartDetails
  export
  globalPosition : LongPressStartDetails -> Offset
  globalPosition this = getField this "globalPosition"

  export
  localPosition : LongPressStartDetails -> Offset
  localPosition this = getField this "localPosition"


namespace LongPressMoveUpdateDetails
  export
  localOffsetFromOrigin : LongPressMoveUpdateDetails -> Offset
  localOffsetFromOrigin this = getField this "localOffsetFromOrigin"

  export
  localPosition : LongPressMoveUpdateDetails -> Offset
  localPosition this = getField this "localPosition"


namespace LongPressEndDetails
  export
  globalPosition : LongPressEndDetails -> Offset
  globalPosition this = getField this "globalPosition"

  export
  localPosition : LongPressEndDetails -> Offset
  localPosition this = getField this "localPosition"
