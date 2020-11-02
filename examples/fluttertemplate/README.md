## The canonical Flutter example app

This example shows how the canonical [Flutter](https://flutter.dev/) example app can be completely defined in Idris.

## Building

Build `lib/main.dart` with:

    $ idris2dart --build fluttertemplate.ipkg

Repair the Flutter app (needs to be done only once) with:

    $ flutter create .

Run it from the command line with:

    $ flutter run

Or from your IDE of choice by navigating to the generated `main` function and starting it from there.

## Setting up a hot reload workflow

I like to use [entr](http://eradman.com/entrproject/):

    $ find lib -iname "*.idr" | entr time idris2dart --build fluttertemplate.ipkg

Combined with the Flutter extension for Visual Studio Code which lets me hot reload after each successful recompilation via `CTRL+F5`.
