module Primitives

export
primDartFastConcat : String
primDartFastConcat = "
$.String Data_Strings_fastConcat($.List ss) {
  final buffer = $.StringBuffer();
  var iterator = ss;
  while (iterator[0] == 1) {
    buffer.write(iterator[1]);
    iterator = iterator[2];
  }
  return buffer.toString();
}
"