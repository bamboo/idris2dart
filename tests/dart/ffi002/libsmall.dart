// @dart = 2.6
class Point {
  final int x;
  final int y;
  Point({this.x, this.y});

  Point.of(this.x, this.y);
}

class Callbacks {
  Object Function(Object) x;
  Object Function(Object) y;
  Callbacks({this.x, this.y});
  Object callX(Object arg) => x(arg);
  Object callY(Object arg) => y(arg);
}
