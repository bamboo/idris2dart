module Primitives

export
primDartFastConcat : String
primDartFastConcat = "
$.String Data_String_fastConcat($.List ss) {
  final buffer = $.StringBuffer();
  while (ss[0] == 1) {
    buffer.write(ss[1]);
    ss = ss[2];
  }
  return buffer.toString();
}"

export
primDartIterableFromList : String
primDartIterableFromList = "
$.Iterable<$.dynamic> Dart_Iterable_fromList($.List es) sync* {
  while (es[0] == 1) {
    yield es[1];
    es = es[2];
  }
}"