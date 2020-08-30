int add(int x, int y) {
  return x + y;
}

int addWithMessage(String msg, int x, int y, [Object idrisWorld]) {
  print("$msg: $x + $y = ${x + y}");
  return x + y;
}

/* f : String -> Int -> String */
String applyFn(String x, int y, dynamic f, [Object idrisWorld]) {
  print("Applying callback to $x $y");
  return f(x)(y);
}

/* f : String -> Int -> PrimIO String */
String applyFnIO(String x, int y, dynamic f, [Object idrisWorld]) {
  print("Applying callback to $x $y");
  return f(x)(y)(idrisWorld);
}
