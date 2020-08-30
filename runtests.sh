cd tests
idris2 --build tests.ipkg && ./build/exec/runtests $(pwd)/../build/exec/idris2dart
