ones : Stream Integer
ones = num :: ones
  where
    num : Integer -- gratuitous where for a regression test!
    num = 1

readNat : IO Nat
readNat = pure $ -- sprinkle some IO to prevent inlining
  case !getLine of
    "42" => Z
    _ => S (S (S (S Z)))

main : IO ()
main = do
  n <- readNat
  print (take n ones)
