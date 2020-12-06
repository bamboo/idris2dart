module Main

%foreign "Dart:cantUseBool,lib.dart"
cantUseBool : Bool -> Int

main : IO ()
main = printLn (cantUseBool True)
