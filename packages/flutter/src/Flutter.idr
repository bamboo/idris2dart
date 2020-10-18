||| FFI definitions for the  Flutter API.
module Flutter

import Dart.FFI

mutual
  %inline
  public export
  Key : Type
  Key = Struct "Key,package:flutter/foundation.dart" []

  %inline
  public export
  MaterialColor : Type
  MaterialColor = Struct "MaterialColor,package:flutter/material.dart" []

  %inline
  public export
  Colors : Type
  Colors = Struct "Colors,package:flutter/material.dart" []

  %inline
  public export
  VisualDensity : Type
  VisualDensity = Struct "VisualDensity,package:flutter/material.dart" []

  %inline
  public export
  ThemeData : Type
  ThemeData = Struct "ThemeData,package:flutter/material.dart" []

  %inline
  public export
  Scaffold : Type
  Scaffold = Struct "Scaffold,package:flutter/material.dart" []

  %inline
  public export
  AppBar : Type
  AppBar = Struct "AppBar,package:flutter/material.dart" []

  %inline
  public export
  Icons : Type
  Icons = Struct "Icons,package:flutter/material.dart" []

  %inline
  public export
  FloatingActionButton : Type
  FloatingActionButton = Struct "FloatingActionButton,package:flutter/material.dart" []

  %inline
  public export
  MaterialApp : Type
  MaterialApp = Struct "MaterialApp,package:flutter/material.dart" []

  %inline
  public export
  IconData : Type
  IconData = Struct "IconData,package:flutter/widgets.dart" []

  %inline
  public export
  Icon : Type
  Icon = Struct "Icon,package:flutter/widgets.dart" []

  %inline
  public export
  Widget : Type
  Widget = Struct "Widget,package:flutter/widgets.dart" []

  %inline
  public export
  BuildContext : Type
  BuildContext = Struct "BuildContext,package:flutter/widgets.dart" []

  %inline
  public export
  Center : Type
  Center = Struct "Center,package:flutter/widgets.dart" []

  %inline
  public export
  Text : Type
  Text = Struct "Text,package:flutter/widgets.dart" []

  %inline
  public export
  Color : Type
  Color = Struct "Color,dart:ui" []

  %inline
  public export
  Canvas : Type
  Canvas = Struct "Canvas,dart:ui" []

  %inline
  public export
  PaintingStyle : Type
  PaintingStyle = Struct "PaintingStyle,dart:ui" []

  %inline
  public export
  Paint : Type
  Paint = Struct "Paint,dart:ui" [
    ("style", PaintingStyle),
    ("strokeWidth", Double)
  ]

  %inline
  public export
  Offset : Type
  Offset = Struct "Offset,dart:ui" [
    ("dx", Double),
    ("dy", Double)
  ]

  %inline
  public export
  Size : Type
  Size = Struct "Size,dart:ui" [
    ("width", Double),
    ("height", Double)
  ]

  %inline
  public export
  Path : Type
  Path = Struct "Path,dart:ui" []

  %inline
  public export
  Velocity : Type
  Velocity = Struct "Velocity,dart:ui" [
    ("pixelsPerSecond", Offset)
  ]

  %inline
  public export
  TapDownDetails : Type
  TapDownDetails = Struct "TapDownDetails,package:flutter/gestures.dart" [
    ("globalPosition", Offset),
    ("localPosition", Offset)
  ]

  %inline
  public export
  TapUpDetails : Type
  TapUpDetails = Struct "TapUpDetails,package:flutter/gestures.dart" [
    ("globalPosition", Offset),
    ("localPosition", Offset)
  ]

  %inline
  public export
  LongPressStartDetails : Type
  LongPressStartDetails = Struct "LongPressStartDetails,package:flutter/gestures.dart" [
    ("globalPosition", Offset),
    ("localPosition", Offset)
  ]

  %inline
  public export
  LongPressMoveUpdateDetails : Type
  LongPressMoveUpdateDetails = Struct "LongPressMoveUpdateDetails,package:flutter/gestures.dart" [
    ("localOffsetFromOrigin", Offset),
    ("localPosition", Offset)
  ]

  %inline
  public export
  LongPressEndDetails : Type
  LongPressEndDetails = Struct "LongPressEndDetails,package:flutter/gestures.dart" [
    ("globalPosition", Offset),
    ("localPosition", Offset)
  ]


namespace Colors
  export
  %foreign "Dart:const Colors.blue,package:flutter/material.dart"
  blue : MaterialColor


namespace VisualDensity
  export
  %foreign "Dart:const VisualDensity.adaptivePlatformDensity,package:flutter/material.dart"
  adaptivePlatformDensity : VisualDensity


namespace ThemeData
  namespace ThemeData.New
    %inline
    public export
    primarySwatch : Param
    primarySwatch = ("primarySwatch", MaterialColor)

    %inline
    public export
    visualDensity : Param
    visualDensity = ("visualDensity", VisualDensity)
    %inline
    public export
    Parameters : Type
    Parameters = ParamList [ThemeData.New.primarySwatch, ThemeData.New.visualDensity]


  %inline
  public export
  new : HasIO io => ThemeData.New.Parameters -> io ThemeData
  new  ps = primIO $ prim__dart_new ThemeData [] ps


namespace Scaffold
  namespace Scaffold.New
    %inline
    public export
    appBar : Param
    appBar = ("appBar", Widget)

    %inline
    public export
    body : Param
    body = ("body", Widget)

    %inline
    public export
    floatingActionButton : Param
    floatingActionButton = ("floatingActionButton", Widget)
    %inline
    public export
    Parameters : Type
    Parameters = ParamList [Scaffold.New.appBar, Scaffold.New.body, Scaffold.New.floatingActionButton]


  %inline
  public export
  new : HasIO io => Scaffold.New.Parameters -> io Scaffold
  new  ps = primIO $ prim__dart_new Scaffold [] ps


namespace AppBar
  namespace AppBar.New
    %inline
    public export
    title : Param
    title = ("title", Widget)
    %inline
    public export
    Parameters : Type
    Parameters = ParamList [AppBar.New.title]


  %inline
  public export
  new : HasIO io => AppBar.New.Parameters -> io AppBar
  new  ps = primIO $ prim__dart_new AppBar [] ps


namespace Icons
  export
  %foreign "Dart:const Icons.add,package:flutter/material.dart"
  add : IconData


namespace FloatingActionButton
  namespace FloatingActionButton.New
    %inline
    public export
    onPressed : Param
    onPressed = ("onPressed", (()))

    %inline
    public export
    tooltip : Param
    tooltip = ("tooltip", String)

    %inline
    public export
    child : Param
    child = ("child", Widget)
    %inline
    public export
    Parameters : Type
    Parameters = ParamList [ FloatingActionButton.New.onPressed
    , FloatingActionButton.New.tooltip
    , FloatingActionButton.New.child ]


  %inline
  public export
  new : HasIO io => FloatingActionButton.New.Parameters -> io FloatingActionButton
  new  ps = primIO $ prim__dart_new FloatingActionButton [] ps


namespace MaterialApp
  namespace MaterialApp.New
    %inline
    public export
    title : Param
    title = ("title", String)

    %inline
    public export
    home : Param
    home = ("home", Widget)

    %inline
    public export
    theme : Param
    theme = ("theme", ThemeData)
    %inline
    public export
    Parameters : Type
    Parameters = ParamList [MaterialApp.New.title, MaterialApp.New.home, MaterialApp.New.theme]


  %inline
  public export
  new : HasIO io => MaterialApp.New.Parameters -> io MaterialApp
  new  ps = primIO $ prim__dart_new MaterialApp [] ps


namespace Icon
  namespace Icon.New
    %inline
    public export
    key : Param
    key = ("key", Key)
    %inline
    public export
    Parameters : Type
    Parameters = ParamList [Icon.New.key]


  %inline
  public export
  new : HasIO io => (icon : IconData) -> Icon.New.Parameters -> io Icon
  new icon ps = primIO $ prim__dart_new Icon [icon] ps


namespace Center
  namespace Center.New
    %inline
    public export
    child : Param
    child = ("child", Widget)
    %inline
    public export
    Parameters : Type
    Parameters = ParamList [Center.New.child]


  %inline
  public export
  new : HasIO io => Center.New.Parameters -> io Center
  new  ps = primIO $ prim__dart_new Center [] ps


namespace Text
  namespace Text.New
    %inline
    public export
    textScaleFactor : Param
    textScaleFactor = ("textScaleFactor", Double)
    %inline
    public export
    Parameters : Type
    Parameters = ParamList [Text.New.textScaleFactor]


  %inline
  public export
  new : HasIO io => (text : String) -> Text.New.Parameters -> io Text
  new text ps = primIO $ prim__dart_new Text [text] ps


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
  %inline
  public export
  new : HasIO io => io Paint
  new  = primIO $ prim__dart_new Paint [] (the (ParamList []) [])

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
  %inline
  public export
  new : HasIO io => (dx : Double) -> (dy : Double) -> io Offset
  new dx dy = primIO $ prim__dart_new Offset [dx, dy] (the (ParamList []) [])

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
  %inline
  public export
  new : HasIO io => io Path
  new  = primIO $ prim__dart_new Path [] (the (ParamList []) [])

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
