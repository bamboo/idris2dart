module Dart.Core.Duration

import Dart.FFI.Elab
import Dart.Core

%language ElabReflection

%runElab importDart [
  package "dart:core" [
    partial' $ class' "Duration" [
      static $ final "Duration" "zero",
      static $ final "int" "microsecondsPerMinute",
      static $ final "int" "microsecondsPerSecond",
      static $ final "int" "microsecondsPerMillisecond",

      final "int" "inSeconds",
      final "int" "inMilliseconds",
      final "int" "inMicroseconds",

      const $ new "" [
        "days" :? "int",
        "hours" :? "int",
        "minutes" :? "int",
        "seconds" :? "int",
        "milliseconds" :? "int",
        "microseconds" :? "int"
      ]
    ]
  ]
]

namespace Duration

  %inline
  public export
  (-) : Duration -> Duration -> Duration
  (-) x y = prim__dart_invoke_pure "-" [] [x, y] Parameters.none

  %inline
  public export
  (+) : Duration -> Duration -> Duration
  (+) x y = prim__dart_invoke_pure "+" [] [x, y] Parameters.none
