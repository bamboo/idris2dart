module Main

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.String

data Point = P Int Int

Pretty Point where
  pretty (P x y) = parens (pretty "Point" <++> pretty x <++> pretty y)

export
makePoint : Int -> Int -> Point
makePoint = P

main : IO ()
main = do
  putStrLn "makePoint 0 $ 0 =>"
  let options = record { layoutPageWidth = AvailablePerLine 7 1 } defaultLayoutOptions
  let doc = the (Doc ()) (pretty points)
  renderIO (layoutPretty options doc)
  where
    origin : Point
    origin = makePoint 0 $ 0
    points : List Point
    points = [origin, origin]
