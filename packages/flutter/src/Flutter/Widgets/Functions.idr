module Flutter.Widgets.Functions

import Dart.FFI
import Flutter.FFI
import public Dart.Core
import public Dart.FFI.Upcast

%inline
export
runApp : {widget : Type} -> IsAssignableFrom Widget widget => widget -> IO ()
runApp w = primIO (prim__dart_invoke "runApp,package:flutter/widgets.dart" [] [w] none)

||| A better alternative to `cast <$> widgetConstructor`.
%inline
export
widget : {a : Type} -> Cast a Widget => IO a -> IO Widget
widget ctor = pure (cast !ctor)

%inline
export
widgets : UpcastList Widget -> Core.List Widget
widgets = fromList . toList
