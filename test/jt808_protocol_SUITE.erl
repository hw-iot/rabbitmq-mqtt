-module(jt808_protocol_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-import(huwo_jt808_frame, [escape/1]).

-define(
   assertBinaryEqual(Expected, Actual),
   ?assertEqual(iolist_to_binary(Expected), iolist_to_binary(Actual))
  ).

all() ->
    [{group, message}].

groups() ->
    [{message, [],
      [message_foo,
       message_escape]}].

message_foo(_) ->
    In = <<16#30, 16#3E, 16#08, 16#3D, 16#55>>,
    Out = <<16#30, 16#3D, 16#02, 16#08, 16#3D, 16#01, 16#55>>,

    Eq = In =:= Out,
    ct:print("message_escape: ~p == ~p is ~p", [In, Out, Eq]).

message_escape(_) ->
    In = <<16#30, 16#3E, 16#08, 16#3D, 16#55>>,
    Out = <<16#30, 16#3D, 16#02, 16#08, 16#3D, 16#01, 16#55>>,
    ?assertBinaryEqual(escape(In), Out),

    In2 = <<16#30, 16#3E, 16#08, 16#3D, 16#55, 16#3E>>,
    Out2 = <<16#30, 16#3D, 16#02, 16#08, 16#3D, 16#01, 16#55, 16#3D, 16#02>>,
    ?assertBinaryEqual(escape(In2), Out2).
