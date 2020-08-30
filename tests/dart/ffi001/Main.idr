||| https://idris2.readthedocs.io/en/latest/ffi/ffi.html
module Main

libsmall : String -> String
libsmall fn = "Dart:" ++ fn ++ ",./libsmall.dart"

||| A pure foreign function.
%foreign (libsmall "add")
add : Int -> Int -> Int

||| An effectful foreign function.
%foreign (libsmall "addWithMessage")
prim__addWithMessage : String -> Int -> Int -> PrimIO Int

addWithMessage : HasIO io => String -> Int -> Int -> io Int
addWithMessage s x y = primIO $ prim__addWithMessage s x y

||| An effectful foreign function that takes a pure callback.
%foreign (libsmall "applyFn")
prim__applyFn : String -> Int -> (String -> Int -> String) -> PrimIO String

applyFn : HasIO io => String -> Int -> (String -> Int -> String) -> io String
applyFn c i f = primIO $ prim__applyFn c i f

||| An effectful foreign function that takes an effectful callback.
%foreign (libsmall "applyFnIO")
prim__applyFnIO :
  String -> Int -> (String -> Int -> PrimIO String) -> PrimIO String

applyFnIO : HasIO io =>
  String -> Int -> (String -> Int -> IO String) -> io String
applyFnIO c i f = primIO $ prim__applyFnIO c i (\s, i => toPrim $ f s i)

pluralise : String -> Int -> String
pluralise str x =
  show x ++ " " ++
    if x == 1
      then str
      else str ++ "s"

pluraliseIO : String -> Int -> IO String
pluraliseIO str x = pure (pluralise str x)

main : IO ()
main = do
  printLn (add 70 24)
  addWithMessage "Sum" 70 24 >>= printLn
  applyFn "Biscuit" 10 pluralise >>= putStrLn
  applyFn "Tree" 1 pluralise >>= putStrLn
  applyFnIO "Biscuit" 10 pluraliseIO >>= putStrLn
  applyFnIO "Tree" 1 pluraliseIO >>= putStrLn
  pure ()
