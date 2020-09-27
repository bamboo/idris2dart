class Point {
  final int x;
  final int y;
  Point({this.x, this.y});

  Point.of(this.x, this.y);
}

class Callbacks {
  Object Function(Object) one;
  Object Function(Object) two;
  Callbacks({this.one, this.two});
  Object callOne(Object arg) => one(arg);
  Object callTwo(Object arg) => two(arg);
}
