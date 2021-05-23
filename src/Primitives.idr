module Primitives

export
primDartFastUnpack : String
primDartFastUnpack = """
$.List Prelude_Types_fastUnpack($.String s) {
  return s.codeUnits.reversed.fold(
    const [0],
    (acc, charCode) => [1, charCode, acc],
  );
}
"""

export
primDartFastPack : String
primDartFastPack = """
$.String Prelude_Types_fastPack($.List list) {
  final buffer = $.StringBuffer();
  while (list[0] == 1) {
    buffer.writeCharCode(list[1]);
    list = list[2];
  }
  return buffer.toString();
}
"""

export
primDartFastConcat : String
primDartFastConcat = """
$.String Prelude_Types_fastConcat($.List list) {
  final buffer = $.StringBuffer();
  while (list[0] == 1) {
    buffer.write(list[1]);
    list = list[2];
  }
  return buffer.toString();
}
"""

export
primDartIterableFromList : String
primDartIterableFromList = """
$.Iterable<$.dynamic> Dart_Iterable_fromList($.List es) sync* {
  while (es[0] == 1) {
    yield es[1];
    es = es[2];
  }
}
"""

export
refTyDef : String
refTyDef = """
class Ref {
  $.dynamic v;
  Ref(this.v);
}
"""