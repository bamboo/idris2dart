module FFI

import Data.List
import Data.String.Extra
import Data.Strings

public export
Lib : Type
Lib = String

public export
data ForeignDartSpec
  = ForeignDartName Lib String

splitAtFirst : Char -> String -> (String, String)
splitAtFirst ch s =
  let (before, rest) = break (== ch) s
  in (before, drop 1 rest)

export
foreignDartSpecFrom : String -> Maybe ForeignDartSpec
foreignDartSpecFrom s =
  if "Dart:" `isPrefixOf` s
    then
      let
        (_, nameCommaLib) = splitAtFirst ':' s
        (name, lib) = splitAtFirst ',' nameCommaLib
      in Just (ForeignDartName lib name)
    else Nothing
