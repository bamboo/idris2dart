int add(int x, int y) {
  return x + y;
}

int addWithMessage(String msg, int x, int y) {
  print("$msg: $x + $y = ${x + y}");
  return x + y;
}

typedef StringFn = String Function(String, int);

String applyFn(String x, int y, StringFn f) {
  print("Applying callback to $x $y");
  return f(x, y);
}

class Point {
  int x;
  int y;
  Point(this.x, this.y);

  void moveTo(int x, int y) {
    this.x = x;
    this.y = y;
  }

  static Point origin() => Point(0, 0);
}