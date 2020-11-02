module Flutter.Widgets.Functions

import Dart.FFI
import Dart.Core
import public Dart.FFI.Upcast
import public Dart.FFI.UpcastList
import Flutter.FFI

%foreign "Dart:runApp,package:flutter/widgets.dart"
prim__runApp : Widget -> PrimIO ()

export
runApp : IsAssignableFrom Widget widget => widget -> IO ()
runApp w = primIO (prim__runApp (upcast w))

export
widgets : UpcastList Widget -> IO (DartList Widget)
widgets ws = into !List.new ws
