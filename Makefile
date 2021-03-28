idris2dart = build/exec/idris2dart

.PHONY: all
all: $(idris2dart) flutter

.PHONY: install
install: all

$(idris2dart): src/*.idr idris2dart.ipkg
	idris2 --build idris2dart.ipkg

dart-ffi-dir = ./packages/dart-ffi
dart-ffi-install-cookie = $(dart-ffi-dir)/build/installed
flutter-dir = ./packages/flutter
flutter-install-cookie = $(flutter-dir)/build/installed
Flutter.idr = $(flutter-dir)/src/Flutter/FFI.idr
flutter-ffi-generator-dir = ./packages/flutter-ffi-generator
flutter-ffi-generator = $(flutter-ffi-generator-dir)/build/exec/flutter-ffi-generator

.PHONY: flutter
flutter: $(flutter-install-cookie)

flutter-ffi-sources = \
	$(wildcard $(flutter-dir)/src/*.idr) \
	$(wildcard $(flutter-dir)/src/**/*.idr)

$(flutter-install-cookie): $(Flutter.idr) $(flutter-ffi-sources) $(dart-ffi-install-cookie)
	cd $(flutter-dir) && idris2 --install ./flutter.ipkg
	touch $(flutter-install-cookie)

dart-ffi-sources = \
	$(wildcard $(dart-ffi-dir)/src/*.idr) \
	$(wildcard $(dart-ffi-dir)/src/**/*.idr) \
	$(wildcard $(dart-ffi-dir)/src/**/**/*.idr)

$(dart-ffi-install-cookie): $(dart-ffi-sources)
	cd $(dart-ffi-dir) && idris2 --install ./dart-ffi.ipkg
	touch $(dart-ffi-install-cookie)

$(Flutter.idr): $(flutter-ffi-generator)
	$(flutter-ffi-generator) > $(Flutter.idr)

flutter-ffi-sources = \
	$(wildcard $(flutter-ffi-generator-dir)/src/*.idr) \
	$(wildcard $(flutter-ffi-generator-dir)/src/**/*.idr)

$(flutter-ffi-generator): $(flutter-ffi-sources)
	cd $(flutter-ffi-generator-dir) && idris2 --build ./flutter-ffi-generator.ipkg

runtests = ./tests/build/exec/runtests

.PHONY: check
check: $(idris2dart) $(runtests) examples
	cd tests && $(realpath $(runtests)) $(realpath $(idris2dart))

$(runtests): ./tests/*.idr ./tests/tests.ipkg $(dart-ffi-install-cookie)
	cd tests && idris2 --build ./tests.ipkg

.PHONY: examples
examples: $(idris2dart)
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
	rm -fr $(flutter-ffi-generator-dir)/build/
