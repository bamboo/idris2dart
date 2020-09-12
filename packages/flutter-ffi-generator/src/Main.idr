import SimpleFFI

dartUI : Lib
dartUI = MkLib "dartUI" "dart:ui"

gestures : Lib
gestures = MkLib "gestures" "package:flutter/gestures.dart"

flutter : Module
flutter = defModule "Flutter" [
  defLib dartUI [
    defClass "Canvas" [
      defIO "drawPath" [
        p "path" "Path",
        p "paint" "Paint"
      ] void
    ],
    defEnum "PaintingStyle" [
      "stroke"
    ],
    defClass "Paint" [
      defConstructor "" [],
      var "style" "PaintingStyle",
      var "strokeWidth" double
    ],
    defClass "Offset" [
      defConstructor "" [p "dx" double, p "dy" double],
      final "dx" double,
      final "dy" double
    ],
    defClass "Size" [
      final "width" double,
      final "height" double
    ],
    defClass "Path" [
      defConstructor "" [],
      defIO "moveTo" [p "x" double, p "y" double] void,
      defIO "lineTo" [p "x" double, p "y" double] void
    ]
  ],
  defLib gestures [
    defClass "TapDownDetails" [
      final "localPosition" "Offset"
    ],
    defClass "LongPressMoveUpdateDetails" [
      final "localOffsetFromOrigin" "Offset",
      final "localPosition" "Offset"
    ]
  ]
]

main : IO ()
main = renderModule flutter
