import 'package:flutter/material.dart';

//@IdrisExport
void runDoodleApp(
  initialState,
  TapUpCallback onTapUp,
  LongPressStartCallback onLongPressStart,
  LongPressMoveUpdateCallback onLongPressMoveUpdate,
  LongPressEndCallback onLongPressEnd,
  PaintCallback onPaint,
) {
  runApp(
    DoodleAppRoot(
      doodleAppSpec: DoodleAppSpec(
        initialState,
        onTapUp,
        onLongPressStart,
        onLongPressMoveUpdate,
        onLongPressEnd,
        onPaint,
      ),
      child: DoodleApp(),
    ),
  );
}

typedef TapUpCallback = Object Function(
  TapUpDetails,
  Object,
);

typedef LongPressStartCallback = Object Function(
  LongPressStartDetails details,
  Object,
);

typedef LongPressMoveUpdateCallback = Object Function(
  LongPressMoveUpdateDetails details,
  Object,
);

typedef LongPressEndCallback = Object Function(
  LongPressEndDetails details,
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
  final TapUpCallback onTapUp;
  final LongPressStartCallback onLongPressStart;
  final LongPressMoveUpdateCallback onLongPressMoveUpdate;
  final LongPressEndCallback onLongPressEnd;
  final PaintCallback onPaint;
  DoodleAppSpec(
    this.initialState,
    this.onTapUp,
    this.onLongPressStart,
    this.onLongPressMoveUpdate,
    this.onLongPressEnd,
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
            onTapUp: (details) {
              modifyState((s) {
                return DoodleAppSpec.of(context).onTapUp(details, s);
              });
            },
            onLongPressStart: (details) {
              modifyState((s) {
                return DoodleAppSpec.of(context).onLongPressStart(details, s);
              });
            },
            onLongPressMoveUpdate: (details) {
              modifyState((s) {
                return DoodleAppSpec.of(context)
                    .onLongPressMoveUpdate(details, s);
              });
            },
            onLongPressEnd: (details) {
              modifyState((s) {
                return DoodleAppSpec.of(context).onLongPressEnd(details, s);
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