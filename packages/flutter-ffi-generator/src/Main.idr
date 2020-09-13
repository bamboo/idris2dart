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
        p "Path" "path",
        p "Paint" "paint"
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
      defConstructor "" [p double "dx", p double "dy"],
      final double "dx",
      final double "dy"
    ],
    defClass "Size" [
      final double "width",
      final double "height"
    ],
    defClass "Path" [
      defConstructor "" [],
      defIO "moveTo" [p double "x", p double "y"] void,
      defIO "lineTo" [p double "x", p double "y"] void
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
