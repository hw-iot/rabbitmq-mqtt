-module(jt808_protocol_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("huwo_jt808_frame.hrl").

-import(huwo_jt808_frame, [escape/1]).

-define(
   assertBinaryEqual(Expected, Actual),
   ?assertEqual(iolist_to_binary(Expected), iolist_to_binary(Actual))
  ).

all() ->
    [{group, message}].

groups() ->
    [{message, [],
      [parse_content,

       message_foo,
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


parse_content(_) ->
    In1 = <<16#08, 16#3D, 16#3E>>,
    Out1 = <<>>,

    ?assertBinaryEqual(huwo_jt808_frame:parse_content(In1, ?FLAG_BOUNDARY), Out1),


    In10 = <<16#3E, 16#08, 16#3D, 16#3E>>,
    Out10 = <<16#08, 16#3D>>,

    ?assertBinaryEqual(huwo_jt808_frame:parse_content(In10, ?FLAG_BOUNDARY), Out10).
