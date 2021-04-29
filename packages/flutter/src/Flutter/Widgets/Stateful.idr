module Flutter.Widgets.Stateful

import Dart.FFI
import Flutter.FFI

%inline
public export
Stateful : Type
Stateful = Struct """
_Stateful,
import 'package:flutter/material.dart' as material;

class _Stateful extends material.StatefulWidget {
  final $.Object initialState;
  final material.Widget Function(_StatefulWidgetState, material.BuildContext) onBuild;

  const _Stateful({material.Key key, this.initialState, this.onBuild}) : super(key: key);

  @$.override
  _StatefulState createState() => _StatefulState(initialState, onBuild);
}

class _StatefulState extends material.State<_Stateful> {
  $.Object state;
  final $.Object Function(_StatefulWidgetState, material.BuildContext) onBuild;

  _StatefulState(this.state, this.onBuild);

  void modify($.Object Function($.Object) f) {
    setState(() {
      state = f(state);
    });
  }

  @$.override
  material.Widget build(material.BuildContext context) {
    return onBuild(_StatefulWidgetState(state, this), context);
  }
}

class _StatefulWidgetState {
  final $.dynamic value;
  final _StatefulState _state;
  _StatefulWidgetState(this.value, this._state);
  void modify($.Object Function($.Object) f) {
    _state.modify(f);
  }
}
""" []

%inline
public export
StatefulWidgetState : Type -> Type
StatefulWidgetState ty = Struct "_StatefulWidgetState" [("value", ty)]

namespace StatefulWidgetState

  %inline
  public export
  get : StatefulWidgetState ty -> ty
  get widgetState = widgetState `getField` "value"

public export
IsAssignableFrom Widget Stateful where

namespace Stateful

  namespace New

    public export
    data Tag : (stateType : Type) -> Type where

    %inline
    public export
    key : {stateType : Type} -> Parameter (Stateful.New.Tag stateType)
    key = mkParameter "key" Key

    %inline
    public export
    initialState : {stateType : Type} -> Parameter (Stateful.New.Tag stateType)
    initialState = mkParameter "initialState" stateType

    %inline
    public export
    onBuild : {stateType : Type} -> Parameter (Stateful.New.Tag stateType)
    onBuild = mkParameter "onBuild" (StatefulWidgetState stateType -> BuildContext -> IO Widget)

    %inline
    public export
    NamedParameters : {stateType : Type} -> Type
    NamedParameters = Parameters [
      Stateful.New.key {stateType = stateType},
      Stateful.New.initialState {stateType = stateType},
      Stateful.New.onBuild {stateType = stateType}
    ]

  %inline
  public export
  new : {stateType : Type} -> Stateful.New.NamedParameters {stateType = stateType} -> Stateful
  new ps = prim__dart_new_const Stateful "" [] ps

  %inline
  public export
  modify : StatefulWidgetState stateType -> (stateType -> stateType) -> IO ()
  modify state f = primIO
    (prim__dart_invoke ".modify" [] [
      the (StatefulWidgetState AnyPtr) (believe_me state),
      the (AnyPtr -> AnyPtr) (believe_me f)
      ] none)
