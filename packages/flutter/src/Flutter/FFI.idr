module Flutter.FFI

import Dart.Core
import Dart.FFI.Elab

%language ElabReflection

%runElab importDart [
  package "package:flutter/foundation.dart" [
    class' "Key" [
    ],
    class' "Listenable" [
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
      final "TextAlign" "textAlign",
      new "" [
        "color" :? "Color",
        -- "decoration" :? "TextDecoration",
        "decorationColor" :? "Color",
        -- "decorationStyle" :? "TextDecorationStyle",
        "decorationThickness" :? "double",
        -- "fontWeight" :? "FontWeight",
        -- "fontStyle" :? "FontStyle",
        -- "textBaseline" :? "TextBaseline",
        "fontFamily" :? "String",
        "fontFamilyFallback" :? "Dart.Core.List" :<> "String",
        "fontSize" :? "double",
        "letterSpacing" :? "double",
        "wordSpacing" :? "double",
        "height" :? "double"
      ]
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
      static $ final "MaterialColor" "orange",
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
    class' "InputBorder" [
      static $ final "InputBorder" "none"
    ],
    class' "InputDecoration" [
      const $ new "" [
        "labelText" :? "String",
        "hintText" :? "String",
        "hintStyle" :? "TextStyle",
        "border" :? "InputBorder"
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
        "semanticsValue" :? "String",
        "valueColor" :? "Animation" :<> "Color"
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
        ],
      generic ["a"] $ static $
        fun ("MaterialStateProperty" :<> "a") "all" [
          "value" :: "a"
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
    class' "TextField" [
      extends "Widget",
      new "" [
        "key" :? "Key",
        "controller" :? "TextEditingController",
        "decoration" :? "InputDecoration",
        "keyboardType" :? "TextInputType",
        "inputFormatters" :? "Dart.Core.List" :<> "TextInputFormatter",
        "textAlign" :? "TextAlign",
        "style" :? "TextStyle"
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
    class' "AnimatedBuilder" [
      extends "Widget",
      const $ new "" [
        "key" :? "Key",
        "animation" :? "Listenable",
        "builder" :? "BuildContext" :-> "Nullable" :<> "Widget" :-> "IO" :<> "Widget",
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
    class' "Navigator" [
      static $ io "NavigatorState" "of" [
        "context" :: "BuildContext",
        "rootNavigator" :? "bool"
      ],
      static $ io "void" "popUntil" [
        "context" :: "BuildContext",
        "predicate" :: "RoutePredicate"
      ]
    ],
    class' "NavigatorState" [
      io "void" "pushNamed" [
        "routeName" :: "String",
        "arguments" :? "Object"
      ]
    ],
    class' "Route" [
    ],
    class' "RoutePredicate" [
    ],
    class' "ModalRoute" [
      static $ fun "RoutePredicate" "withName" [
        "name" :: "String"
      ]
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
      ],
      var "TextEditingValue" "value"
    ],
    class' "Widget" [
    ],
    class' "WidgetBuilder" [
    ],
    class' "TransitionBuilder" [
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
      const $ new "" [
        "value" :: "int"
      ],
      fun "Color" "withOpacity" [
        "opacity" :: "double"
      ]
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
  ],
  package "package:flutter/services.dart" [
    class' "FilteringTextInputFormatter" [
      static $ final "TextInputFormatter" "digitsOnly"
    ],
    class' "TextEditingValue" [
      final "String" "text"
    ],
    class' "TextInputFormatter" [
    ],
    class' "TextInputType" [
      static $ final "TextInputType" "datetime",
      static $ final "TextInputType" "emailAddress",
      static $ final "TextInputType" "multiline",
      static $ final "TextInputType" "name",
      static $ final "TextInputType" "number",
      static $ final "TextInputType" "phone",
      static $ final "TextInputType" "streetAddress",
      static $ final "TextInputType" "text",
      static $ final "TextInputType" "url",
      static $ final "TextInputType" "visiblePassword"
    ]
  ],
  package "package:flutter/scheduler.dart" [
    class' "TickerProvider" [
    ]
  ],
  package "package:flutter/animation.dart" [
    enum "AnimationBehavior" [
      "normal",
      "preserve"
    ],
    class' "AnimationController" [
      extends ("Animation" :<> "double"),
      extends "Listenable",
      new "" [
        "value" :? "double",
        "duration" :? "Duration",
        "reverseDuration" :? "Duration",
        "debugLabel" :? "String",
        "lowerBound" :? "double",
        "upperBound" :? "double",
        "animationBehavior" :? "AnimationBehavior",
        "vsync" :? "TickerProvider"
      ],
      io "void" "animateTo" ["value" :: "double"],
      generic ["u"] $
        io ("Animation" :<> "u") "drive" [
          "animatable" :: "Animatable" :<> "u"
        ],
      io "void" "dispose" []
    ],
    generic ["a"] $
      class' "Animatable" [
      ],
    generic ["a"] $
      class' "Animation" [
        getter "a" "value"
      ],
    class' "ColorTween" [
      extends ("Animatable" :<> "Color"),
      extends "Listenable",
      new "" [
        "begin" :? "Color",
        "end" :? "Color"
      ]
    ],
    class' "Curve" [
    ],
    class' "Curves" [
      static $ final "Curve" "bounceIn",
      static $ final "Curve" "bounceInOut",
      static $ final "Curve" "bounceOut",
      static $ final "Curve" "decelerate",
      static $ final "Curve" "ease",
      static $ final "Curve" "easeIn",
      static $ final "Curve" "easeInBack",
      static $ final "Curve" "easeInCirc",
      static $ final "Curve" "easeInCubic",
      static $ final "Curve" "easeInExpo",
      static $ final "Curve" "easeInOut",
      static $ final "Curve" "easeInOutBack",
      static $ final "Curve" "easeInOutCirc",
      static $ final "Curve" "easeInOutCubic",
      static $ final "Curve" "easeInOutCubicEmphasized",
      static $ final "Curve" "easeInOutExpo",
      static $ final "Curve" "easeInOutQuad",
      static $ final "Curve" "easeInOutQuart",
      static $ final "Curve" "easeInOutQuint",
      static $ final "Curve" "easeInOutSine",
      static $ final "Curve" "easeInQuad",
      static $ final "Curve" "easeInQuart",
      static $ final "Curve" "easeInQuint",
      static $ final "Curve" "easeInSine",
      static $ final "Curve" "easeInToLinear",
      static $ final "Curve" "easeOut",
      static $ final "Curve" "easeOutBack",
      static $ final "Curve" "easeOutCirc",
      static $ final "Curve" "easeOutCubic",
      static $ final "Curve" "easeOutExpo",
      static $ final "Curve" "easeOutQuad",
      static $ final "Curve" "easeOutQuart",
      static $ final "Curve" "easeOutQuint",
      static $ final "Curve" "easeOutSine",
      static $ final "Curve" "elasticIn",
      static $ final "Curve" "elasticInOut",
      static $ final "Curve" "elasticOut",
      static $ final "Curve" "fastLinearToSlowEaseIn",
      static $ final "Curve" "fastOutSlowIn",
      static $ final "Curve" "linear",
      static $ final "Curve" "linearToEaseOut",
      static $ final "Curve" "slowMiddle"
    ],
    class' "CurveTween" [
      extends ("Animatable" :<> "double"),
      extends "Listenable",
      new "" [
        "curve" :? "Curve"
      ]
    ],
    generic ["a"] $
      class' "TweenSequence" [
        -- extends ("Animatable" :<> "a"),
        extends "Listenable",
        new "" [
          "items" :: "Dart.Core.List" :<> ("TweenSequenceItem" :<> "a")
        ]
      ],
    generic ["a"] $
      class' "TweenSequenceItem" [
        const $ new "" [
          "tween" :? "Animatable" :<> "a",
          "weight" :? "double"
        ]
      ]
  ]
]

-- TODO: fix `extends` for generic types
public export
IsAssignableFrom (Animatable a) (TweenSequence a) where