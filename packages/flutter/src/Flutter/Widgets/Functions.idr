module Flutter.Widgets.Functions

import Dart.FFI
import Flutter.FFI
import public Dart.Core
import public Dart.FFI.Upcast

%inline
export
runApp : {widget : Type} -> IsAssignableFrom Widget widget => widget -> IO ()
runApp w = primIO (prim__dart_invoke "runApp,package:flutter/widgets.dart" [] [w] none)

||| A simpler alternative to `pure (cast widget)`.
%inline
export
widget : {a : Type} -> Cast a Widget => a -> IO Widget
widget w = pure (cast w)

%inline
export
widgets : UpcastList Widget -> Core.List Widget
widgets = fromList . toList
