module Flutter.Combinators

import Dart.FFI
import Dart.Core
import public Dart.FFI.UpcastList
import Flutter.FFI

export
widgets : UpcastList Widget -> IO (DartList Widget) 
widgets ws = into !List.new ws

