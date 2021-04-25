module Dart.Core.Set

import Dart.FFI.Elab
import Dart.Core

%language ElabReflection

%runElab importDart [
  package "dart:core" [
    partial' $
      generic ["e"] $
        class' "Set" [
          io "bool" "contains" ["element" :: "e"]
        ]
  ]
]