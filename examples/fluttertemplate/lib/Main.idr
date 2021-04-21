module Main

import Flutter

appTitle : String
appTitle = "Idris Demo Home Page"

appHome : IO Stateful
appHome = Stateful.new [initialState @= 0, onBuild @= build]
  where
    build : StatefulWidgetState Int -> BuildContext -> IO Widget
    build state context = cast <$> Scaffold.new [
      appBar @=> !(AppBar.new [
        title @=> !(Text.new appTitle [])
      ]),
      body @=> !(Center.new [
        child @=> !(Column.new [
          mainAxisAlignment @= MainAxisAlignment.center,
          children @= widgets [
            !(Text.new "You have pushed the button this many times:" []),
            !(Text.new (show (get state)) [
              style @= headline4 (textTheme !(Theme.of context))
            ])
          ]
        ])
      ]),
      floatingActionButton @=> !(FloatingActionButton.new [
        tooltip @= "Increment",
        child @=> !(Icon.new Icons.add []),
        onPressed @= modify state (+ 1)
      ])
    ]

app : IO Stateless
app = Stateless.new [onBuild @= build]
  where
    build : BuildContext -> IO Widget
    build _ = cast <$> MaterialApp.new [
      title @= appTitle,
      home @=> !appHome,
      theme @= !(ThemeData.new [
-- This is the theme of your application.
--
-- Try running your application with "flutter run". You'll see the
-- application has a blue toolbar. Then, without quitting the app, try
-- changing the primarySwatch below to Colors.green and then invoke
-- "hot reload" (press "r" in the console where you ran "flutter run",
-- or simply save your changes to "hot reload" in a Flutter IDE).
-- Notice that the counter didn't reset back to zero; the application
-- is not restarted.
        primarySwatch @= Colors.blue
      ])
    ]

main : IO ()
main = runApp !app
