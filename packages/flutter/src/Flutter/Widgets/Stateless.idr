module Flutter.Widgets.Stateless

import Dart.FFI
import Flutter.FFI

%inline
public export
Stateless : Type
Stateless = Struct """
_Stateless,
import 'package:flutter/material.dart' as material;

class _Stateless extends material.StatelessWidget {
  final material.Widget Function(material.BuildContext) onBuild;
  _Stateless({material.Key key, this.onBuild}) : super(key: key);
  @$.override
  material.Widget build(material.BuildContext context) {
    return onBuild(context);
  }
}
""" []

public export
IsAssignableFrom Widget Stateless where

namespace Stateless

  namespace New

    public export
    data Tag : Type where

    %inline
    public export
    key : Parameter Stateless.New.Tag
    key = mkParameter "key" Key

    %inline
    public export
    onBuild : Parameter Stateless.New.Tag
    onBuild = mkParameter "onBuild" (BuildContext -> IO Widget)

    %inline
    public export
    NamedParameters : Type
    NamedParameters = Parameters [
      Stateless.New.key,
      Stateless.New.onBuild
    ]

  %inline
  public export
  new : Stateless.New.NamedParameters -> IO Stateless
  new ps = primIO (prim__dart_new Stateless "" [] ps)
