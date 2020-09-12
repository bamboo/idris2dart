import 'package:flutter/material.dart';

//@IdrisExport
void runDoodleApp(
  initialState,
  TapDownCallback onTapDown,
  LongPressMoveUpdateCallback onLongPressMoveUpdate,
  PaintCallback onPaint,
) {
  runApp(
    DoodleAppRoot(
      doodleAppSpec: DoodleAppSpec(
        initialState,
        onTapDown,
        onLongPressMoveUpdate,
        onPaint,
      ),
      child: DoodleApp(),
    ),
  );
}

typedef TapDownCallback = Object Function(
  TapDownDetails,
  Object,
);

typedef LongPressMoveUpdateCallback = Object Function(
  LongPressMoveUpdateDetails details,
  Object,
);

typedef PaintCallback = void Function(
  Canvas,
  Size,
  Object,
);

class DoodleAppRoot extends InheritedWidget {
  static DoodleAppRoot of(BuildContext context) {
    return context
        .getElementForInheritedWidgetOfExactType<DoodleAppRoot>()
        .widget;
  }

  DoodleAppRoot({
    Key key,
    @required Widget child,
    this.doodleAppSpec,
  }) : super(key: key, child: child);

  final DoodleAppSpec doodleAppSpec;

  @override
  bool updateShouldNotify(DoodleAppRoot oldWidget) {
    return doodleAppSpec != oldWidget.doodleAppSpec;
  }
}

class DoodleAppSpec {
  static DoodleAppSpec of(BuildContext context) {
    return DoodleAppRoot.of(context).doodleAppSpec;
  }

  final Object initialState;
  final TapDownCallback onTapDown;
  final LongPressMoveUpdateCallback onLongPressMoveUpdate;
  final PaintCallback onPaint;
  DoodleAppSpec(
    this.initialState,
    this.onTapDown,
    this.onLongPressMoveUpdate,
    this.onPaint,
  );
}

class DoodleApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Idris Flutter Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
        visualDensity: VisualDensity.adaptivePlatformDensity,
      ),
      home: DoodlePage(
        title: 'Idris Flutter Demo',
        initialState: DoodleAppSpec.of(context).initialState,
      ),
    );
  }
}

class DoodlePage extends StatefulWidget {
  final initialState;

  DoodlePage({Key key, this.title, this.initialState}) : super(key: key);

  final String title;

  @override
  _DoodlePageState createState() => _DoodlePageState(initialState);
}

class _DoodlePageState extends State<DoodlePage> {
  Object state;
  _DoodlePageState(this.state);

  void modifyState(Object Function(Object) f) {
    setState(() {
      this.state = f(this.state);
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(widget.title),
      ),
      body: Center(
        child: CustomPaint(
          child: GestureDetector(
            onTapDown: (details) {
              modifyState((s) {
                return DoodleAppSpec.of(context).onTapDown(details, s);
              });
            },
            onLongPressMoveUpdate: (details) {
              modifyState((s) {
                return DoodleAppSpec.of(context)
                    .onLongPressMoveUpdate(details, s);
              });
            },
          ),
          painter: DoodlePainter(
            onPaint: DoodleAppSpec.of(context).onPaint,
            state: state,
          ),
        ),
      ),
    );
  }
}

class DoodlePainter extends CustomPainter {
  final PaintCallback onPaint;
  final Object state;
  DoodlePainter({this.onPaint, this.state});

  @override
  void paint(Canvas canvas, Size size) {
    onPaint(canvas, size, state);
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) {
    return oldDelegate != this;
  }
}
