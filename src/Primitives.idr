module Primitives

export
primDartFastConcat : String
primDartFastConcat = "
$.String Data_String_fastConcat($.List strings) {
  final buffer = $.StringBuffer();
  var iterator = strings;
  while (iterator[0] == 1) {
    buffer.write(iterator[1]);
    iterator = iterator[2];
  }
  return buffer.toString();
}
"