module Flutter.Widgets.Stateless

import Dart.FFI.Elab
import Flutter.FFI

%language ElabReflection

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

%runElab importDart [
  package "" [
    partial' $
      class' "Stateless" [
        extends "Widget",
        new "" [
          "key" :? "Key",
          "onBuild" :? "BuildContext" :-> "IO" :<> "Widget"
        ]
      ]
  ]
]