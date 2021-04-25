module Flutter.FFI

import Dart.Core
import Dart.FFI.Elab

%language ElabReflection

%runElab importDart [
  package "package:flutter/foundation.dart" [
    class' "Key" [
    ]
  ],
  package "package:flutter/rendering.dart" [
    enum "MainAxisSize" [
      "max",
      "min"
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
    class' "EdgeInsets" [
      extends "EdgeInsetsGeometry",
      const $ new "all" [
        "value" :: "double"
      ]
    ],
    class' "EdgeInsetsGeometry" [
    ],
    class' "ShapeBorder" [
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
      const $ new "" [
        "backgroundColor" :? "MaterialStateProperty" :<> "Color",
        "foregroundColor" :? "MaterialStateProperty" :<> "Color",
        "textStyle" :? "MaterialStateProperty" :<> "TextStyle",
        "overlayColor" :? "MaterialStateProperty" :<> "Color",
        "shadowColor" :? "MaterialStateProperty" :<> "Color"
      ]
    ],
    class' "Card" [
      extends "Widget",
      const $ new "" [
        "key" :? "Key",
        "color" :? "Color",
        "shadowColor" :? "Color",
        "elevation" :? "double",
        "shape" :? "ShapeBorder",
        "borderOnForeground" :? "bool",
        "margin" :? "EdgeInsetsGeometry",
        "clipBehavior" :? "Clip",
        "child" :? "Widget",
        "semanticContainer" :? "bool"
      ]
    ],
    class' "CircularProgressIndicator" [
      extends "Widget",
      const $ new "" [
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
      const $ new "" [
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
      const $ new "" [
        "onPressed" :? "IO" :<> "()",
        "tooltip" :? "String",
        "icon" :? "Widget",
        "alignment" :? "AlignmentGeometry"
      ]
    ],
    class' "InputDecoration" [
      const $ new "" [
        "labelText" :? "String",
        "hintText" :? "String"
      ]
    ],
    class' "LinearProgressIndicator" [
      extends "Widget",
      const $ new "" [
        "key" :? "Key",
        "value" :? "double",
        "backgroundColor" :? "Color",
        "minHeight" :? "double",
        "semanticsLabel" :? "String",
        "semanticsValue" :? "String"
        -- Animation<Color?>? valueColor,
      ]
    ],
    class' "MaterialApp" [
      extends "Widget",
      const $ new "" [
        "title" :? "String",
        "home" :? "Widget",
        "theme" :? "ThemeData",
        "routes" :? "Dart.Core.Map" :<> "String" :<> "WidgetBuilder"
      ]
    ],
    class' "MaterialColor" [
      extends "Color",
      final "int" "alpha",
      final "int" "blue",
      final "int" "green",
      final "int" "red",
      final "double" "opacity",
      final "Color" "shade50",
      final "Color" "shade100",
      final "Color" "shade200",
      final "Color" "shade300",
      final "Color" "shade400",
      final "Color" "shade500",
      final "Color" "shade600",
      final "Color" "shade700",
      final "Color" "shade800",
      final "Color" "shade900"
    ],
    enum "MaterialState" [
      "disabled",
      "dragged",
      "error",
      "focused",
      "hovered",
      "pressed",
      "selected"
    ],
    generic ["a"] $ class' "MaterialStateProperty" [
      generic ["a"] $ static $
        io ("MaterialStateProperty" :<> "a") "resolveWith" [
          "resolver" :: "Dart.Core.Set" :<> "MaterialState" :-> "IO" :<> "a"
        ]
    ],
    class' "Scaffold" [
      extends "Widget",
      const $ new "" [
        "appBar" :? "Widget",
        "body" :? "Widget",
        "floatingActionButton" :? "Widget",
        "backgroundColor" :? "Color"
      ]
    ],
    class' "Slider" [
      extends "Widget",
      const $ new "" [
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
      const $ new "" [
        "onPressed" :? "IO" :<> "()",
        "style" :? "ButtonStyle",
        "child" :? "Widget"
      ],
      static $ fun "ButtonStyle" "styleFrom" [
        "primary" :? "Color",
        "backgroundColor" :? "Color"
      ]
    ],
    class' "TextFormField" [
      extends "FormField",
      extends "Widget",
      new "" [
        "key" :? "Key",
        "controller" :? "TextEditingController",
        "initialValue" :? "String",
        "decoration" :? "InputDecoration"
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
    class' "Align" [
      const $ new "" [
        "key" :? "Key",
        "alignment" :? "AlignmentGeometry",
        "widthFactor" :? "double",
        "heightFactor" :? "double",
        "child" :? "Widget"
      ]
    ],
    enum "AutovalidateMode" [
      "always",
      "disabled",
      "onUserInteraction"
    ],
    class' "BuildContext" [
    ],
    class' "Center" [
      extends "Widget",
      const $ new "" [
        "child" :? "Widget"
      ]
    ],
    class' "Column" [
      extends "Widget",
      new "" [
        "children" :? "Dart.Core.List" :<> "Widget",
        "mainAxisAlignment" :? "MainAxisAlignment",
        "mainAxisSize" :? "MainAxisSize"
      ]
    ],
    class' "CustomPaint" [
      extends "Widget",
      const $ new "" [
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
      const $ new "" [
        "child" :? "Widget"
      ]
    ],
    class' "FittedBox" [
      extends "Widget",
      const $ new "" [
        "key" :? "Key",
        "child" :? "Widget",
        "fit" :? "BoxFit",
        "alignment" :? "AlignmentGeometry",
        "clipBehavior" :? "Clip"
      ]
    ],
    class' "Form" [
      extends "Widget",
      const $ new "" [
        "child" :? "Widget",
        "onChanged" :? "IO" :<> "()",
        "autovalidateMode" :? "AutovalidateMode"
      ]
    ],
    class' "FormField" [
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
      const $ new "" [
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
    class' "Padding" [
      extends "Widget",
      const $ new "" [
        "key" :? "Key",
        "padding" :? "EdgeInsetsGeometry",
        "child" :? "Widget"
      ]
    ],
    class' "SizedBox" [
      extends "Widget",
      const $ new "" [
        "key" :? "Key",
        "width" :? "double",
        "height" :? "double",
        "child" :? "Widget"
      ]
    ],
    class' "Text" [
      extends "Widget",
      const $ new "" [
        "text" :: "String",
        "textScaleFactor" :? "Double",
        "style" :? "TextStyle",
        "textAlign" :? "TextAlign"
      ]
    ],
    class' "TextEditingController" [
      new "" [
        "text" :? "String"
      ]
    ],
    class' "Widget" [
    ],
    class' "WidgetBuilder" [
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
      const $ new "" ["dx" :: "Double", "dy" :: "Double"],
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
