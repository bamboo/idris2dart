module Flutter.Widgets.Painter

import Dart.FFI
import Flutter.FFI

%inline
public export
Painter : Type
Painter = Struct "_Painter,

import 'package:flutter/material.dart' as material;

class _Painter extends material.CustomPainter {

  final void Function(material.Canvas, material.Size) onPaint;

  _Painter({this.onPaint});

  @$.override
  void paint(material.Canvas canvas, material.Size size) {
    onPaint(canvas, size);
  }

  @$.override
  $.bool shouldRepaint(covariant material.CustomPainter oldDelegate) {
    return oldDelegate != this;
  }
}" []

public export
IsAssignableFrom CustomPainter Painter where

namespace Painter
  namespace New
    data Tag : Type where

    %inline
    public export
    onPaint : Parameter Painter.New.Tag
    onPaint = mkParameter "onPaint" (Canvas -> Size -> IO ())

    %inline
    public export
    NamedParameters : Type 
    NamedParameters = Parameters [Painter.New.onPaint]

  %inline
  export
  new : Painter.New.NamedParameters -> IO Painter
  new ps = primIO (prim__dart_new Painter "" [] ps)