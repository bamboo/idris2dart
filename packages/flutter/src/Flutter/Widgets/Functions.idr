module Flutter.Widgets.Functions

import Dart.FFI
import public Dart.Core -- for DartList
import public Dart.FFI.Upcast
import public Dart.FFI.UpcastList
import Flutter.FFI

%inline
export
runApp : {widget : Type} -> IsAssignableFrom Widget widget => widget -> IO ()
runApp w = primIO (prim__dart_invoke "runApp,package:flutter/widgets.dart" [w] none)

export
widgets : UpcastList Widget -> IO (DartList Widget)
widgets ws = into !List.new ws
