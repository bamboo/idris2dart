import 'package:flutter/material.dart';
import 'dart:ui';

void runDoodleApp(
  initialState,
  TapDownCallback onTapDown,
  PaintCallback onPaint,
) {
  runApp(DoodleApp(DoodleAppDef(initialState, onTapDown, onPaint)));
}

typedef TapDownCallback = Object Function(double, double, Object);

typedef PaintCallback = void Function(Canvas, double, double, Object);

class DoodleAppDef {
  final Object initialState;
  final TapDownCallback onTapDown;
  final PaintCallback onPaint;
  DoodleAppDef(this.initialState, this.onTapDown, this.onPaint);
}

class DoodleApp extends StatelessWidget {
  final DoodleAppDef doodleAppDef;
  DoodleApp(this.doodleAppDef);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Idris Flutter Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
        visualDensity: VisualDensity.adaptivePlatformDensity,
      ),
      home: MyHomePage(
        title: 'Idris Flutter Demo',
        doodleAppDef: doodleAppDef,
      ),
    );
  }
}

class MyHomePage extends StatefulWidget {
  final DoodleAppDef doodleAppDef;
  MyHomePage({Key key, this.doodleAppDef, this.title}) : super(key: key);

  final String title;

  @override
  _MyHomePageState createState() => _MyHomePageState(doodleAppDef);
}

class _MyHomePageState extends State<MyHomePage> {
  final DoodleAppDef doodleAppDef;
  Object state;
  _MyHomePageState(this.doodleAppDef) {
    state = this.doodleAppDef.initialState;
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
              setState(
                () {
                  this.state = doodleAppDef.onTapDown(
                    details.localPosition.dx,
                    details.localPosition.dy,
                    state,
                  );
                },
              );
            },
          ),
          painter: LambdaPainter(
            doodleAppDef: doodleAppDef,
            state: state,
          ),
        ),
      ),
    );
  }
}

class LambdaPainter extends CustomPainter {
  final DoodleAppDef doodleAppDef;
  final Object state;
  LambdaPainter({this.doodleAppDef, this.state});

  @override
  void paint(Canvas canvas, Size size) {
    doodleAppDef.onPaint(canvas, size.width, size.height, state);
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) {
    return oldDelegate != this;
  }
}
