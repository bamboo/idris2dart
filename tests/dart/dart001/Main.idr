module Main

data Point = P Int Int

Show Point where
  show (P x y) = "(Point " ++ show x ++ " " ++ show y ++ ")"

export
makePoint : Int -> Int -> Point
makePoint = P

main : IO ()
main = do
  putStr "makePoint 0 $ 0 => "
  printLn origin
  where 
    origin : Point
    origin = makePoint 0 $ 0
