## idris2dart

An [Idris 2](https://idris2.readthedocs.io/en/latest/) code generator that outputs [Dart](https://dart.dev/) code.

### Why

To explore what cross-platform app development with Idris 2 powered by [Flutter](https://flutter.dev/) can look like and maybe build something beautiful along the way.

### Status

- [x] data types, pattern matching, etc
- [x] basic Char, String and numeric primitives
- [x] [bidirectional FFI](./tests/dart/ffi001/Main.idr)
- [x] delay/force
- [x] remaining cast primitives
- [x] bounded int operations on `Bits*` values
- [ ] remaining IO primitives
- [ ] `IOArray` primitives

### Building

1. Install the latest Idris 2 (must include the _idris2api_ package)
2. `make all`
3. Add `./build/exec/idris2dart` to your `PATH` or create an alias

### Using

`idris2dart` is a fully functional Idris 2 environment except it comes with a single code generator, `dart`.

For example, to compile an Idris module to Dart, use:

```
$ idris2dart Main.idr -o main.dart
```
