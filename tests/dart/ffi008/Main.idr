module Main

import Dart.Core
import Dart.FFI
import Dart.FFI.Elab

%inline
Foreign : Type
Foreign = Struct """
Foreign,

class Foreign {
  final Function() callback;
  Foreign({this.callback});
  Foreign.of(this.callback);
  void report() {
    $.print(callback == null ? 'callback is null' : 'callback is not null');
    callback?.call();
  }
}
""" []

%language ElabReflection

%runElab importDart [
  package "" [
    partial' $ class' "Foreign" [
      new "" [
        "callback" :? "IO" :<> "()"
      ],
      new "of" [
        "callback" :: "IO" :<> "()"
      ],
      io "void" "report" []
    ]
  ]
]

main : IO ()
main = do
  f <- Foreign.new [callback @= unsafeNull]
  f @. report

  f <- Foreign.new [callback @= putStrLn "foo"]
  f @. report

  f <- Foreign.of unsafeNull
  f @. report

  f <- Foreign.of (putStrLn "bar")
  f @. report