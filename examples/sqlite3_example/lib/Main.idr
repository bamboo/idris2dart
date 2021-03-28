module Main

import Dart.Core
import Dart.FFI.Elab

%language ElabReflection

%runElab importDart [
  package "package:sqlite3/sqlite3.dart" [
    final "Sqlite3" "sqlite3",
    class' "Sqlite3" [
      final "Version" "version",
      io "Database" "openInMemory" []
    ],
    class' "Version" [
    ],
    class' "Database" [
      io "void" "execute" ["sql" :: "String"],
      io "PreparedStatement" "prepare" ["sql" :: "String"],
      io "ResultSet" "select" ["sql" :: "String", "parameters" :: "Dart.Core.List" :<> "Object"],
      io "void" "dispose" []
    ],
    class' "PreparedStatement" [
      io "void" "execute" ["parameters" :: "Dart.Core.List" :<> "Object"],
      io "void" "dispose" []
    ],
    class' "ResultSet" [
      extends ("Iterable" :<> "Row")
    ],
    class' "Row" [
    ]
  ]
]

%inline
objectList : UpcastList Object -> Core.List Object
objectList = fromList . toList

main : IO ()
main = do
  putStrLn ("Using sqlite3 " ++ !(sqlite3 @. version @. toString))
  db <- sqlite3 @. openInMemory
  db @. execute """
    CREATE TABLE artists (
      id INTEGER NOT NULL PRIMARY KEY,
      name TEXT NOT NULL
    );
  """
  stmt <- db @. prepare "INSERT INTO artists (name) VALUES (?)"
  stmt @. execute (objectList ["The Beatles"])
  stmt @. execute (objectList ["Led Zeppelin"])
  stmt @. execute (objectList ["The Who"])
  stmt @. execute (objectList ["Nirvana"])
  dispose stmt

  resultSet <- db @. select "SELECT * FROM artists WHERE name LIKE ?" (objectList ["The %"])

  resultSet @. forEach {element = Row} print

  dispose db