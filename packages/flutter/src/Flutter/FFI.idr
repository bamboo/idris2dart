||| FFI definitions for the  Flutter.FFI API.
module Flutter.FFI

import Dart.Core
import Dart.FFI

mutual
  %inline
  public export
  Key : Type
  Key = Struct "Key,package:flutter/foundation.dart" []

  %inline
  public export
  TextStyle : Type
  TextStyle = Struct "TextStyle,package:flutter/painting.dart" []

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
  TextTheme : Type
  TextTheme = Struct "TextTheme,package:flutter/material.dart" [
    ("headline1", TextStyle),
    ("headline2", TextStyle),
    ("headline3", TextStyle),
    ("headline4", TextStyle)
  ]

  %inline
  public export
  Theme : Type
  Theme = Struct "Theme,package:flutter/material.dart" []

  %inline
  public export
  ThemeData : Type
  ThemeData = Struct "ThemeData,package:flutter/material.dart" [
    ("textTheme", TextTheme)
  ]

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
  Slider : Type
  Slider = Struct "Slider,package:flutter/material.dart" []

  %inline
  public export
  MaterialApp : Type
  MaterialApp = Struct "MaterialApp,package:flutter/material.dart" []

  %inline
  public export
  MainAxisAlignment : Type
  MainAxisAlignment = Struct "MainAxisAlignment,package:flutter/widgets.dart" []

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
  Column : Type
  Column = Struct "Column,package:flutter/widgets.dart" []

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
  %foreign "Dart:const Colors.black,package:flutter/material.dart"
  black : MaterialColor

  export
  %foreign "Dart:const Colors.blue,package:flutter/material.dart"
  blue : MaterialColor

  export
  %foreign "Dart:const Colors.brown,package:flutter/material.dart"
  brown : MaterialColor

  export
  %foreign "Dart:const Colors.cyan,package:flutter/material.dart"
  cyan : MaterialColor

  export
  %foreign "Dart:const Colors.green,package:flutter/material.dart"
  green : MaterialColor

  export
  %foreign "Dart:const Colors.grey,package:flutter/material.dart"
  grey : MaterialColor

  export
  %foreign "Dart:const Colors.purple,package:flutter/material.dart"
  purple : MaterialColor

  export
  %foreign "Dart:const Colors.red,package:flutter/material.dart"
  red : MaterialColor

  export
  %foreign "Dart:const Colors.transparent,package:flutter/material.dart"
  transparent : MaterialColor

  export
  %foreign "Dart:const Colors.white,package:flutter/material.dart"
  white : MaterialColor

  export
  %foreign "Dart:const Colors.yellow,package:flutter/material.dart"
  yellow : MaterialColor


namespace VisualDensity
  export
  %foreign "Dart:const VisualDensity.adaptivePlatformDensity,package:flutter/material.dart"
  adaptivePlatformDensity : VisualDensity


namespace TextTheme
  export
  headline1 : TextTheme -> TextStyle
  headline1 this = getField this "headline1"

  export
  headline2 : TextTheme -> TextStyle
  headline2 this = getField this "headline2"

  export
  headline3 : TextTheme -> TextStyle
  headline3 this = getField this "headline3"

  export
  headline4 : TextTheme -> TextStyle
  headline4 this = getField this "headline4"


namespace Theme
  %foreign "Dart:Theme.of,package:flutter/material.dart"
  prim__of : (context : BuildContext) -> PrimIO ThemeData

  export
  of_ : HasIO io => (context : BuildContext) -> io ThemeData
  of_ context = primIO $ prim__of context


namespace ThemeData
  namespace New
    data Tag : Type where

    %inline
    public export
    primarySwatch : Parameter ThemeData.New.Tag
    primarySwatch = mkParameter "primarySwatch" MaterialColor

    %inline
    public export
    visualDensity : Parameter ThemeData.New.Tag
    visualDensity = mkParameter "visualDensity" VisualDensity
    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [ThemeData.New.primarySwatch, ThemeData.New.visualDensity]


  %inline
  public export
  new : HasIO io => ThemeData.New.NamedParameters -> io ThemeData
  new  ps = primIO $ prim__dart_new ThemeData [] ps

  export
  textTheme : ThemeData -> TextTheme
  textTheme this = getField this "textTheme"


namespace Scaffold
  export
  IsAssignableFrom Widget Scaffold where

  namespace New
    data Tag : Type where

    %inline
    public export
    appBar : Parameter Scaffold.New.Tag
    appBar = mkParameter "appBar" Widget

    %inline
    public export
    body : Parameter Scaffold.New.Tag
    body = mkParameter "body" Widget

    %inline
    public export
    floatingActionButton : Parameter Scaffold.New.Tag
    floatingActionButton = mkParameter "floatingActionButton" Widget
    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [Scaffold.New.appBar, Scaffold.New.body, Scaffold.New.floatingActionButton]


  %inline
  public export
  new : HasIO io => Scaffold.New.NamedParameters -> io Scaffold
  new  ps = primIO $ prim__dart_new Scaffold [] ps


namespace AppBar
  export
  IsAssignableFrom Widget AppBar where

  namespace New
    data Tag : Type where

    %inline
    public export
    title : Parameter AppBar.New.Tag
    title = mkParameter "title" Widget
    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [AppBar.New.title]


  %inline
  public export
  new : HasIO io => AppBar.New.NamedParameters -> io AppBar
  new  ps = primIO $ prim__dart_new AppBar [] ps


namespace Icons
  export
  %foreign "Dart:const Icons.add,package:flutter/material.dart"
  add : IconData

  export
  %foreign "Dart:const Icons.play_arrow,package:flutter/material.dart"
  play_arrow : IconData

  export
  %foreign "Dart:const Icons.stop,package:flutter/material.dart"
  stop : IconData

  export
  %foreign "Dart:const Icons.stop_circle,package:flutter/material.dart"
  stop_circle : IconData

  export
  %foreign "Dart:const Icons.stop_circle_outlined,package:flutter/material.dart"
  stop_circle_outlined : IconData

  export
  %foreign "Dart:const Icons.stop_circle_rounded,package:flutter/material.dart"
  stop_circle_rounded : IconData

  export
  %foreign "Dart:const Icons.stop_circle_sharp,package:flutter/material.dart"
  stop_circle_sharp : IconData

  export
  %foreign "Dart:const Icons.stop_outlined,package:flutter/material.dart"
  stop_outlined : IconData

  export
  %foreign "Dart:const Icons.stop_rounded,package:flutter/material.dart"
  stop_rounded : IconData


namespace FloatingActionButton
  export
  IsAssignableFrom Widget FloatingActionButton where

  namespace New
    data Tag : Type where

    %inline
    public export
    onPressed : Parameter FloatingActionButton.New.Tag
    onPressed = mkParameter "onPressed" (IO ())

    %inline
    public export
    tooltip : Parameter FloatingActionButton.New.Tag
    tooltip = mkParameter "tooltip" String

    %inline
    public export
    child : Parameter FloatingActionButton.New.Tag
    child = mkParameter "child" Widget
    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [ FloatingActionButton.New.onPressed
    , FloatingActionButton.New.tooltip
    , FloatingActionButton.New.child ]


  %inline
  public export
  new : HasIO io => FloatingActionButton.New.NamedParameters -> io FloatingActionButton
  new  ps = primIO $ prim__dart_new FloatingActionButton [] ps


namespace Slider
  export
  IsAssignableFrom Widget Slider where

  namespace New
    data Tag : Type where

    %inline
    public export
    value : Parameter Slider.New.Tag
    value = mkParameter "value" Double

    %inline
    public export
    onChanged : Parameter Slider.New.Tag
    onChanged = mkParameter "onChanged" (Double -> IO ())

    %inline
    public export
    onChangeStart : Parameter Slider.New.Tag
    onChangeStart = mkParameter "onChangeStart" (Double -> IO ())

    %inline
    public export
    onChangeEnd : Parameter Slider.New.Tag
    onChangeEnd = mkParameter "onChangeEnd" (Double -> IO ())

    %inline
    public export
    min : Parameter Slider.New.Tag
    min = mkParameter "min" Double

    %inline
    public export
    max : Parameter Slider.New.Tag
    max = mkParameter "max" Double

    %inline
    public export
    divisions : Parameter Slider.New.Tag
    divisions = mkParameter "divisions" Int

    %inline
    public export
    label : Parameter Slider.New.Tag
    label = mkParameter "label" String
    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [ Slider.New.value
    , Slider.New.onChanged
    , Slider.New.onChangeStart
    , Slider.New.onChangeEnd
    , Slider.New.min
    , Slider.New.max
    , Slider.New.divisions
    , Slider.New.label ]


  %inline
  public export
  new : HasIO io => Slider.New.NamedParameters -> io Slider
  new  ps = primIO $ prim__dart_new Slider [] ps


namespace MaterialApp
  export
  IsAssignableFrom Widget MaterialApp where

  namespace New
    data Tag : Type where

    %inline
    public export
    title : Parameter MaterialApp.New.Tag
    title = mkParameter "title" String

    %inline
    public export
    home : Parameter MaterialApp.New.Tag
    home = mkParameter "home" Widget

    %inline
    public export
    theme : Parameter MaterialApp.New.Tag
    theme = mkParameter "theme" ThemeData
    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [MaterialApp.New.title, MaterialApp.New.home, MaterialApp.New.theme]


  %inline
  public export
  new : HasIO io => MaterialApp.New.NamedParameters -> io MaterialApp
  new  ps = primIO $ prim__dart_new MaterialApp [] ps


namespace MainAxisAlignment
  export
  %foreign "Dart:const MainAxisAlignment.start,package:flutter/widgets.dart"
  start : MainAxisAlignment
  export
  %foreign "Dart:const MainAxisAlignment.end,package:flutter/widgets.dart"
  end : MainAxisAlignment
  export
  %foreign "Dart:const MainAxisAlignment.center,package:flutter/widgets.dart"
  center : MainAxisAlignment
  export
  %foreign "Dart:const MainAxisAlignment.spaceBetween,package:flutter/widgets.dart"
  spaceBetween : MainAxisAlignment
  export
  %foreign "Dart:const MainAxisAlignment.spaceAround,package:flutter/widgets.dart"
  spaceAround : MainAxisAlignment
  export
  %foreign "Dart:const MainAxisAlignment.spaceEvenly,package:flutter/widgets.dart"
  spaceEvenly : MainAxisAlignment


namespace Icon
  export
  IsAssignableFrom Widget Icon where

  namespace New
    data Tag : Type where

    %inline
    public export
    key : Parameter Icon.New.Tag
    key = mkParameter "key" Key
    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [Icon.New.key]


  %inline
  public export
  new : HasIO io => (icon : IconData) -> Icon.New.NamedParameters -> io Icon
  new icon ps = primIO $ prim__dart_new Icon [icon] ps


namespace Center
  export
  IsAssignableFrom Widget Center where

  namespace New
    data Tag : Type where

    %inline
    public export
    child : Parameter Center.New.Tag
    child = mkParameter "child" Widget
    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [Center.New.child]


  %inline
  public export
  new : HasIO io => Center.New.NamedParameters -> io Center
  new  ps = primIO $ prim__dart_new Center [] ps


namespace Column
  export
  IsAssignableFrom Widget Column where

  namespace New
    data Tag : Type where

    %inline
    public export
    children : Parameter Column.New.Tag
    children = mkParameter "children" (DartList Widget)

    %inline
    public export
    mainAxisAlignment : Parameter Column.New.Tag
    mainAxisAlignment = mkParameter "mainAxisAlignment" MainAxisAlignment
    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [Column.New.children, Column.New.mainAxisAlignment]


  %inline
  public export
  new : HasIO io => Column.New.NamedParameters -> io Column
  new  ps = primIO $ prim__dart_new Column [] ps


namespace Text
  export
  IsAssignableFrom Widget Text where

  namespace New
    data Tag : Type where

    %inline
    public export
    textScaleFactor : Parameter Text.New.Tag
    textScaleFactor = mkParameter "textScaleFactor" Double

    %inline
    public export
    style : Parameter Text.New.Tag
    style = mkParameter "style" TextStyle
    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [Text.New.textScaleFactor, Text.New.style]


  %inline
  public export
  new : HasIO io => (text : String) -> Text.New.NamedParameters -> io Text
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
  new  = primIO $ prim__dart_new Paint [] (the (Parameters {tag = Void} []) [])

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
  new dx dy = primIO $ prim__dart_new Offset [dx, dy] (the (Parameters {tag = Void} []) [])

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
  new  = primIO $ prim__dart_new Path [] (the (Parameters {tag = Void} []) [])

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
