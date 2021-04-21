idris2dart = build/exec/idris2dart

.PHONY: all
all: $(idris2dart) flutter

.PHONY: install
install: all

$(idris2dart): $(shell find src -type f -iname "*.idr") idris2dart.ipkg
	idris2 --build idris2dart.ipkg

dart-ffi-dir = ./packages/dart-ffi
dart-ffi-install-cookie = $(dart-ffi-dir)/build/installed
dart-ffi-sources = $(shell find $(dart-ffi-dir) -type f -iname "*.idr")

flutter-dir = ./packages/flutter
flutter-install-cookie = $(flutter-dir)/build/installed
flutter-sources = $(shell find $(flutter-dir) -type f -iname "*.idr")

.PHONY: flutter
flutter: $(flutter-install-cookie)

$(flutter-install-cookie): $(flutter-sources) $(dart-ffi-install-cookie)
	cd $(flutter-dir) && idris2 --install ./flutter.ipkg
	touch $(flutter-install-cookie)

$(dart-ffi-install-cookie): $(dart-ffi-sources)
	cd $(dart-ffi-dir) && idris2 --install ./dart-ffi.ipkg
	touch $(dart-ffi-install-cookie)

runtests = ./tests/build/exec/runtests

.PHONY: check
check: test examples

.PHONY: test
test: $(idris2dart) $(runtests)
	cd tests && $(realpath $(runtests)) $(realpath $(idris2dart))

$(runtests): ./tests/*.idr ./tests/tests.ipkg $(dart-ffi-install-cookie)
	cd tests && idris2 --build ./tests.ipkg

.PHONY: examples
examples: $(idris2dart) flutter
	cd examples/sqlite3_example && $(realpath $(idris2dart)) --build sqlite3-example.ipkg
	cd examples/fluttertemplate && $(realpath $(idris2dart)) --build fluttertemplate.ipkg
	cd examples/flutterdoodle && $(realpath $(idris2dart)) --build flutterdoodle.ipkg

.PHONY: clean
clean:
	rm -fr ./build/
	rm -fr ./examples/sqlite3_example/build/
	rm -fr ./examples/fluttertemplate/build/
	rm -fr ./examples/flutterdoodle/build/
	rm -fr ./tests/build/
	rm -fr $(dart-ffi-dir)/build/
	rm -fr $(flutter-dir)/build/
