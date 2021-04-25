module Flutter.Widgets.Painter

import Dart.FFI.Elab
import Flutter.FFI

%language ElabReflection

%inline
public export
Painter : Type
Painter = Struct """
_Painter,

import 'package:flutter/material.dart' as material;

class _Painter extends material.CustomPainter {

  final void Function(material.Canvas, material.Size) onPaint;

  const _Painter({@material.required this.onPaint});

  @$.override
  void paint(material.Canvas canvas, material.Size size) {
    onPaint(canvas, size);
  }

  @$.override
  $.bool shouldRepaint(covariant material.CustomPainter oldDelegate) {
    return oldDelegate != this;
  }
}
""" []

%runElab importDart [
  package "" [
    partial' $
      class' "Painter" [
        extends "CustomPainter",
        const $ new "" [
          "onPaint" :? "Canvas" :-> "Size" :-> "IO" :<> "()"
        ]
      ]
  ]
]