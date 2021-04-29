module Dart.Core.String

import Dart.FFI.Elab
import Dart.Core

%language ElabReflection

%runElab importDart [
  package "dart:core" [
    primitive $ class' "String" [
      final "bool" "isEmpty",
      final "bool" "isNotEmpty",

      fun "String" "padLeft" [
        "width" :: "int",
        "padding" :: "String"
      ],

      fun "String" "padRight" [
        "width" :: "int",
        "padding" :: "String"
      ]
    ]
  ]
]