||| A simplified Wadler style pretty printing library without the pretty part.
module Printer

import Data.String.Extra
import System.File

export
data Doc
  = Nil
  | LineBreak
  | Text String
  | Nest Int Doc
  | Seq Doc Doc

public export
Semigroup Doc where
  Nil <+> y = y
  x <+> Nil = x
  x <+> y = Seq x y

public export
Monoid Doc where
  neutral = Nil

public export
Show Doc where
  show doc = case doc of
    Nil => "Nil"
    LineBreak => "LineBreak"
    Text s => show s
    Seq x y => "(" ++ show x ++ " <+> " ++ show y ++ ")"
    Nest i d => "(Nest " ++ show i ++ " " ++ show d ++ ")"

export
empty : Doc
empty = Nil

export
line : Doc
line = LineBreak

export
text : String -> Doc
text "" = Nil
text "\n" = LineBreak
text s = Text s

public export %inline
shown : Show a => a -> Doc
shown a = text (show a)

export
FromString Doc where
  fromString = text

export
nest : Int -> Doc -> Doc
nest = Nest

export 
indented : Doc -> Doc
indented d = nest 2 (line <+> d) <+> line

export
sepBy : Doc -> List Doc -> Doc
sepBy sep [] = empty
sepBy sep (d::ds) = foldl (\acc, e => acc <+> sep <+> e) d ds

export
vcat : List Doc -> Doc
vcat = sepBy line

export
hcat : List Doc -> Doc
hcat = sepBy empty

export
commaSep : List Doc -> Doc
commaSep = sepBy ", "

export
paren : Doc -> Doc
paren d = "(" <+> d <+> ")"

export
tupled : List Doc -> Doc
tupled = paren . commaSep

export
block : Doc -> Doc
block b = " {" <+> indented b <+> "}"

export
semi : Doc
semi = text ";"

export
dot : Doc
dot = text "."

ok : Either FileError ()
ok = Right ()

writeDocTo : HasIO io => File -> Doc -> (indent : String) -> io (Either FileError ())
writeDocTo f doc i = case doc of
  Text s => fPutStr f s
  LineBreak => do
    Right () <- fPutStr f "\n" | e => pure e 
    if length i > 0
      then fPutStr f i
      else pure ok
  Seq x y => do
    Right () <- writeDocTo f x i | e => pure e 
    writeDocTo f y i
  Nest w d => writeDocTo f d (nSpaces (fromInteger (cast w) + length i))
  Nil => pure ok 
  where
    nSpaces : Nat -> String
    nSpaces i = replicate i ' '

||| Writes a [Doc] to a file
export
writeDocToFile : HasIO io => (filepath: String) -> (doc : Doc) -> io (Either FileError ()) 
writeDocToFile fn doc = do
  Right h  <- openFile fn WriteTruncate
    | Left err => pure (Left err)
  Right () <- writeDocTo h doc "" 
    | Left err => do
      closeFile h
      pure (Left err)
  closeFile h
  pure ok 
