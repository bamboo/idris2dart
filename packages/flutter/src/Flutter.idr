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
  TapDownDetails : Type
  TapDownDetails = Struct "TapDownDetails,package:flutter/gestures.dart" [
    ("localPosition", Offset)
  ]

  public export
  LongPressMoveUpdateDetails : Type
  LongPressMoveUpdateDetails = Struct "LongPressMoveUpdateDetails,package:flutter/gestures.dart" [
    ("localOffsetFromOrigin", Offset),
    ("localPosition", Offset)
  ]


namespace Canvas
  %foreign "Dart:.drawPath"
  prim__drawPath : (this : Canvas) -> (path : Path) -> (paint : Paint) -> PrimIO ()

  export
  drawPath : HasIO io => (path : Path) -> (paint : Paint) -> (this : Canvas) -> io ()
  drawPath path paint this = primIO $ this.prim__drawPath path paint


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
  style this = this.getField "style"

  export
  setStyle : PaintingStyle -> Paint -> IO ()
  setStyle value this = this.setField "style" value

  export
  strokeWidth : Paint -> Double
  strokeWidth this = this.getField "strokeWidth"

  export
  setStrokeWidth : Double -> Paint -> IO ()
  setStrokeWidth value this = this.setField "strokeWidth" value


namespace Offset
  %foreign "Dart:Offset,dart:ui"
  prim__Offset : (dx : Double) -> (dy : Double) -> PrimIO Offset

  export
  new : HasIO io => (dx : Double) -> (dy : Double) -> io Offset
  new dx dy = primIO $ prim__Offset dx dy

  export
  dx : Offset -> Double
  dx this = this.getField "dx"

  export
  dy : Offset -> Double
  dy this = this.getField "dy"


namespace Size
  export
  width : Size -> Double
  width this = this.getField "width"

  export
  height : Size -> Double
  height this = this.getField "height"


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
  moveTo x y this = primIO $ this.prim__moveTo x y

  %foreign "Dart:.lineTo"
  prim__lineTo : (this : Path) -> (x : Double) -> (y : Double) -> PrimIO ()

  export
  lineTo : HasIO io => (x : Double) -> (y : Double) -> (this : Path) -> io ()
  lineTo x y this = primIO $ this.prim__lineTo x y


namespace TapDownDetails
  export
  localPosition : TapDownDetails -> Offset
  localPosition this = this.getField "localPosition"


namespace LongPressMoveUpdateDetails
  export
  localOffsetFromOrigin : LongPressMoveUpdateDetails -> Offset
  localOffsetFromOrigin this = this.getField "localOffsetFromOrigin"

  export
  localPosition : LongPressMoveUpdateDetails -> Offset
  localPosition this = this.getField "localPosition"
