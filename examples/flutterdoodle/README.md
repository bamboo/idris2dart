## A sample Flutter application 

This example demonstrates how to use [Idris FFI definitions](./lib/Flutter.idr) to integrate with [Flutter](https://flutter.dev/).

## Building

Build `lib/main.dart` with:

   $ idris2dart --build flutterdoodle.ipkg

Repair the Flutter app (needs to be done only once) with:

   $ flutter create .
   
Run it:

   $ flutter run

## Setting up a hot-reload workflow

I like to use [entr](http://eradman.com/entrproject/):

   $ find lib -iname "*.idr" | entr time idris2dart --build flutterdoodle.ipkg
