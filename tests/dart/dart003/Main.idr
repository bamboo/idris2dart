module Main

readString : String -> IO String
readString d = pure d

test : String -> IO ()
test s = do
  s' <- readString s
  printLn (the Double (cast s'), the Integer (cast s'), the Int (cast s'))

bigInteger : IO Integer
bigInteger = pure 9223372036854775808

main : IO ()
main = do
  test "42.5"
  test "-42.5"
  test "42"
  test "-42"
  test "foo"
  printLn !bigInteger