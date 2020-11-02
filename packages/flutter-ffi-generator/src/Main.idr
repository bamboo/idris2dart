import SimpleFFI

dartUI : Lib
dartUI = MkLib "dartUI" "dart:ui"

gestures : Lib
gestures = MkLib "gestures" "package:flutter/gestures.dart"

widgets : Lib
widgets = MkLib "widgets" "package:flutter/widgets.dart"

material : Lib
material = MkLib "material" "package:flutter/material.dart"

foundation : Lib
foundation = MkLib "foundation" "package:flutter/foundation.dart"

flutter : Module
flutter = defModule "Flutter.FFI" [
  defLib foundation [
    defClass "Key" [
    ]
  ],
  defLib material [
    defClass "MaterialColor" [
    ],
    defClass "Colors" [
      defConst "MaterialColor" "black",
      defConst "MaterialColor" "blue",
      defConst "MaterialColor" "brown",
      defConst "MaterialColor" "cyan",
      defConst "MaterialColor" "green",
      defConst "MaterialColor" "grey",
      defConst "MaterialColor" "purple",
      defConst "MaterialColor" "red",
      defConst "MaterialColor" "transparent",
      defConst "MaterialColor" "white",
      defConst "MaterialColor" "yellow"
    ],
    defClass "VisualDensity" [
      defConst "VisualDensity" "adaptivePlatformDensity"
    ],
    defClass "ThemeData" [
      defConstructor "" [
        named "MaterialColor" "primarySwatch",
        named "VisualDensity" "visualDensity"
      ]
    ],
    defClass "Scaffold" [
      extends "Widget",
      defConstructor "" [
        named "Widget" "appBar",
        named "Widget" "body",
        named "Widget" "floatingActionButton"
      ]
    ],
    defClass "AppBar" [
      extends "Widget",
      defConstructor "" [
        named "Widget" "title"
      ]
    ],
    defClass "Icons" [
      defConst "IconData" "add"
    ],
    defClass "FloatingActionButton" [
      extends "Widget",
      defConstructor "" [
        named (function [] void) "onPressed",
        named string "tooltip",
        named "Widget" "child"
      ]
    ],
    defClass "MaterialApp" [
      extends "Widget",
      defConstructor "" [
        named string "title",
        named "Widget" "home",
        named "ThemeData" "theme"
      ]
    ]
  ],
  defLib widgets [
    defEnum "MainAxisAlignment" [
      "start", "end", "center", "spaceBetween", "spaceAround", "spaceEvenly"
    ],
    defClass "IconData" [
    ],
    defClass "Icon" [
      extends "Widget",
      defConstructor "" [
        positional "IconData" "icon",
        named "Key" "key"
      ]
    ],
    defClass "Widget" [
    ],
    defClass "BuildContext" [
    ],
    defClass "Center" [
      extends "Widget",
      defConstructor "" [
        named "Widget" "child"
      ]
    ],
    defClass "Column" [
      extends "Widget",
      defConstructor "" [
        named (listOf "Widget") "children",
        named "MainAxisAlignment" "mainAxisAlignment"
      ]
    ],
    defClass "Text" [
      extends "Widget",
      defConstructor "" [
        positional string "text",
        named double "textScaleFactor"
      ]
    ]
  ],
  defLib dartUI [
    defClass "Color" [
    ],
    defClass "Canvas" [
      defIO "drawPath" [
        positional "Path" "path",
        positional "Paint" "paint"
      ] void
    ],
    defEnum "PaintingStyle" [
      "stroke"
    ],
    defClass "Paint" [
      defConstructor "" [],
      var "PaintingStyle" "style",
      var double "strokeWidth"
    ],
    defClass "Offset" [
      defConstructor "" [positional double "dx", positional double "dy"],
      final double "dx",
      final double "dy"
    ],
    defClass "Size" [
      final double "width",
      final double "height"
    ],
    defClass "Path" [
      defConstructor "" [],
      defIO "moveTo" [positional double "x", positional double "y"] void,
      defIO "lineTo" [positional double "x", positional double "y"] void
    ],
    defClass "Velocity" [
      final "Offset" "pixelsPerSecond"
    ]
  ],
  defLib gestures [
    defClass "TapDownDetails" [
      final "Offset" "globalPosition",
      final "Offset" "localPosition"
    ],
    defClass "TapUpDetails" [
      final "Offset" "globalPosition",
      final "Offset" "localPosition"
    ],
    defClass "LongPressStartDetails" [
      final "Offset" "globalPosition",
      final "Offset" "localPosition"
    ],
    defClass "LongPressMoveUpdateDetails" [
      final "Offset" "localOffsetFromOrigin",
      final "Offset" "localPosition"
    ],
    defClass "LongPressEndDetails" [
      final "Offset" "globalPosition",
      final "Offset" "localPosition"
    ]
  ]
]

main : IO ()
main = renderModule flutter
