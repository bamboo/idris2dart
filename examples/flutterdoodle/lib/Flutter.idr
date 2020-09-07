||| FFI definitions for a subset of the Flutter API.
module Flutter

import System.FFI

dartUi : String -> String
dartUi fn = "Dart:" ++ fn ++ ",dart:ui"

public export
Canvas : Type
Canvas = AnyPtr

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
Path : Type
Path = AnyPtr

namespace PaintingStyle
  export
  %foreign (dartUi "const PaintingStyle.stroke")
  stroke : PaintingStyle

namespace Paint
  export
  setStyle : PaintingStyle -> Paint -> IO ()
  setStyle s p = p.setField "style" s

  export
  setStrokeWidth : Double -> Paint -> IO ()
  setStrokeWidth w p = p.setField "strokeWidth" w

%foreign "Dart:.drawPath"
prim__drawPath : Canvas -> Path -> Paint -> PrimIO ()

namespace Canvas
  export
  drawPath : HasIO io => Path -> Paint -> Canvas -> io ()
  drawPath path paint canvas = primIO $ canvas.prim__drawPath path paint

%foreign (dartUi "Path")
prim__Path : PrimIO Path

%foreign "Dart:.moveTo"
prim__moveTo : Path -> Double -> Double -> PrimIO ()

%foreign "Dart:.lineTo"
prim__lineTo : Path -> Double -> Double -> PrimIO ()

namespace Path
  export
  new : HasIO io => io Path
  new = primIO prim__Path

  export
  moveTo : HasIO io => Double -> Double -> Path -> io ()
  moveTo x y p = primIO $ p.prim__moveTo x y

  export
  lineTo : HasIO io => Double -> Double -> Path -> io ()
  lineTo x y p = primIO $ p.prim__lineTo x y

%foreign (dartUi "Paint")
prim__Paint : PrimIO Paint

namespace Paint
  export
  new : HasIO io => io Paint
  new = primIO prim__Paint
