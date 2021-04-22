module Flutter.FFI

import Dart.Core
import Dart.FFI.Elab

%language ElabReflection

%runElab importDart [
  package "package:flutter/foundation.dart" [
    class' "Key" [
    ]
  ],
  package "package:flutter/painting.dart" [
    class' "Alignment" [
      extends "AlignmentGeometry",
      static $ final "Alignment" "centerLeft",
      static $ final "Alignment" "centerRight"
    ],
    class' "AlignmentGeometry" [
    ],
    enum "BoxFit" [
      "contain", "cover", "fill", "fitHeight", "fitWidth", "none", "scaleDown"
    ],
    class' "TextStyle" [
      final "TextAlign" "textAlign"
    ]
  ],
  package "package:flutter/material.dart" [
    class' "AppBar" [
      extends "Widget",
      new "" [
        "title" :? "Widget"
      ]
    ],
    class' "ButtonStyle" [
    ],
    class' "CircularProgressIndicator" [
      extends "Widget",
      new "" [
        "key" :? "Key"
      ]
    ],
    class' "Colors" [
      static $ final "MaterialColor" "black",
      static $ final "MaterialColor" "blue",
      static $ final "MaterialColor" "brown",
      static $ final "MaterialColor" "cyan",
      static $ final "MaterialColor" "green",
      static $ final "MaterialColor" "grey",
      static $ final "MaterialColor" "purple",
      static $ final "MaterialColor" "red",
      static $ final "MaterialColor" "transparent",
      static $ final "MaterialColor" "white",
      static $ final "MaterialColor" "yellow"
    ],
    class' "FloatingActionButton" [
      extends "Widget",
      new "" [
        "onPressed" :? "IO" :<> "()",
        "tooltip" :? "String",
        "child" :? "Widget"
      ]
    ],
    class' "Icons" [
      static $ final "IconData" "add",
      static $ final "IconData" "add_circle_outline",
      static $ final "IconData" "remove",
      static $ final "IconData" "remove_circle_outline",
      static $ final "IconData" "play_arrow",
      static $ final "IconData" "stop",
      static $ final "IconData" "stop_circle",
      static $ final "IconData" "stop_circle_outlined",
      static $ final "IconData" "stop_circle_rounded",
      static $ final "IconData" "stop_circle_sharp",
      static $ final "IconData" "stop_outlined",
      static $ final "IconData" "stop_rounded"
    ],
    class' "IconButton" [
      extends "Widget",
      new "" [
        "onPressed" :? "IO" :<> "()",
        "tooltip" :? "String",
        "icon" :? "Widget",
        "alignment" :? "AlignmentGeometry"
      ]
    ],
    class' "MaterialApp" [
      extends "Widget",
      new "" [
        "title" :? "String",
        "home" :? "Widget",
        "theme" :? "ThemeData"
      ]
    ],
    class' "MaterialColor" [
    ],
    class' "Scaffold" [
      extends "Widget",
      new "" [
        "appBar" :? "Widget",
        "body" :? "Widget",
        "floatingActionButton" :? "Widget"
      ]
    ],
    class' "Slider" [
      extends "Widget",
      new "" [
        "value" :? "Double",
        "onChanged" :? "Double" :-> "IO" :<> "()",
        "onChangeStart" :? "Double" :-> "IO" :<> "()",
        "onChangeEnd" :? "Double" :-> "IO" :<> "()",
        "min" :? "Double",
        "max" :? "Double",
        "divisions" :? "int",
        "label" :? "String"
      ]
    ],
    class' "TextButton" [
      extends "Widget",
      new "" [
        "onPressed" :? "IO" :<> "()",
        "style" :? "ButtonStyle",
        "child" :? "Widget"
      ],
      static $ fun "ButtonStyle" "styleFrom" [
        "primary" :? "Color",
        "backgroundColor" :? "Color"
      ]
    ],
    class' "TextTheme" [
      final "TextStyle" "subtitle1",
      final "TextStyle" "subtitle2",
      final "TextStyle" "headline1",
      final "TextStyle" "headline2",
      final "TextStyle" "headline3",
      final "TextStyle" "headline4"
    ],
    class' "Theme" [
      static $ io "ThemeData" "of" ["context" :: "BuildContext"]
    ],
    class' "ThemeData" [
      new "" [
        "primarySwatch" :? "MaterialColor",
        "visualDensity" :? "VisualDensity"
      ],
      final "TextTheme" "textTheme"
    ],
    class' "VisualDensity" [
      static $ final "VisualDensity" "adaptivePlatformDensity"
    ]
  ],
  package "package:flutter/widgets.dart" [
    class' "BuildContext" [
    ],
    class' "Center" [
      extends "Widget",
      new "" [
        "child" :? "Widget"
      ]
    ],
    class' "Column" [
      extends "Widget",
      new "" [
        "children" :? "Dart.Core.List" :<> "Widget",
        "mainAxisAlignment" :? "MainAxisAlignment"
      ]
    ],
    class' "CustomPaint" [
      extends "Widget",
      new "" [
        "key" :? "Key",
        "child" :? "Widget",
        "painter" :? "CustomPainter",
        "foregroundPainter" :? "CustomPainter",
        "size" :? "Size",
        "isComplex" :? "bool",
        "willChange" :? "bool"
      ]
    ],
    class' "CustomPainter" [
    ],
    class' "Expanded" [
      extends "Widget",
      new "" [
        "child" :? "Widget"
      ]
    ],
    class' "FittedBox" [
      extends "Widget",
      new "" [
        "key" :? "Key",
        "child" :? "Widget",
        "fit" :? "BoxFit ",
        "alignment" :? "AlignmentGeometry",
        "clipBehavior" :? "Clip"
    ],
    class' "GestureDetector" [
      extends "Widget",
      new "" [
        "key" :? "Key",
        "child" :? "Widget",
        "onTapDown" :? "TapDownDetails" :-> "IO" :<> "()",
        "onTapUp" :? "TapUpDetails" :-> "IO" :<> "()",
        "onTap" :? "IO" :<> "()",
        "onTapCancel" :? "IO" :<> "()",
        "onSecondaryTap" :? "IO" :<> "()",
        "onSecondaryTapCancel" :? "IO" :<> "()",
        "onDoubleTap" :? "IO" :<> "()",
        "onDoubleTapCancel" :? "IO" :<> "()",
        "onSecondaryTapDown" :? "TapDownDetails" :-> "IO" :<> "()",
        "onSecondaryTapUp" :? "TapUpDetails" :-> "IO" :<> "()",
        "onLongPress" :? "IO" :<> "()",
        "onLongPressStart" :? "LongPressStartDetails" :-> "IO" :<> "()",
        "onLongPressMoveUpdate" :? "LongPressMoveUpdateDetails" :-> "IO" :<> "()",
        "onLongPressEnd" :? "LongPressEndDetails" :-> "IO" :<> "()"
      ]
    ],
    class' "Icon" [
      extends "Widget",
      new "" [
        "icon" :: "IconData",
        "key" :? "Key"
      ]
    ],
    class' "IconData" [
    ],
    enum "MainAxisAlignment" [
      "start", "end", "center", "spaceBetween", "spaceAround", "spaceEvenly"
    ],
    class' "Row" [
      extends "Widget",
      new "" [
        "children" :? "Dart.Core.List" :<> "Widget",
        "mainAxisAlignment" :? "MainAxisAlignment"
      ]
    ],
    class' "Text" [
      extends "Widget",
      new "" [
        "text" :: "String",
        "textScaleFactor" :? "Double",
        "style" :? "TextStyle",
        "textAlign" :? "TextAlign"
      ]
    ],
    class' "Widget" [
    ]
  ],
  package "dart:ui" [
    enum "AppLifecycleState" [
      "detached",
      "inactive",
      "paused",
      "resumed"
    ],
    class' "Canvas" [
      io "void" "drawPath" [
        "path" :: "Path",
        "paint" :: "Paint"
      ]
    ],
    enum "Clip" [
      "antiAlias", "antiAliasWithSaveLayer", "hardEdge", "none"
    ],
    class' "Color" [
    ],
    class' "Offset" [
      new "" ["dx" :: "Double", "dy" :: "Double"],
      final "Double" "dx",
      final "Double" "dy"
    ],
    class' "Paint" [
      new "" [],
      var "PaintingStyle" "style",
      var "Double" "strokeWidth"
    ],
    enum "PaintingStyle" [
      "stroke"
    ],
    class' "Path" [
      new "" [],
      io "void" "moveTo" ["x" :: "Double", "y" :: "Double"],
      io "void" "lineTo" ["x" :: "Double", "y" :: "Double"]
    ],   
    class' "Size" [
      final "Double" "width",
      final "Double" "height"
    ],
    enum "TextAlign" [
      "left",
      "right",
      "center",
      "start",
      "end",
      "justify"
    ],
    class' "Velocity" [
      final "Offset" "pixelsPerSecond"
    ]
  ],
  package "package:flutter/gestures.dart" [
    class' "LongPressEndDetails" [
      final "Offset" "globalPosition",
      final "Offset" "localPosition"
    ],
    class' "LongPressMoveUpdateDetails" [
      final "Offset" "localOffsetFromOrigin",
      final "Offset" "localPosition"
    ],
    class' "LongPressStartDetails" [
      final "Offset" "globalPosition",
      final "Offset" "localPosition"
    ],
    class' "TapDownDetails" [
      final "Offset" "globalPosition",
      final "Offset" "localPosition"
    ],
    class' "TapUpDetails" [
      final "Offset" "globalPosition",
      final "Offset" "localPosition"
    ]
  ]
]
