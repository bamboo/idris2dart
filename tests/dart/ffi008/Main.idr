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
  void report() {
    $.print(callback == null ? 'callback is null' : 'callback is not null');
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
      io "void" "report" []
    ]
  ]
]

main : IO ()
main = do
  f <- Foreign.new [callback @= unsafeNull]
  f @. report