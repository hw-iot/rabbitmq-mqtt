-module(jt808_protocol_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("huwo_jt808_frame.hrl").

-import(huwo_jt808_frame, [escape/2, unescape/2]).

all() ->
    [{group, message}].

groups() ->
    [{message, [],
      [test_escape_7e,
       test_escape_3e]}].

test_escape_7e(_) ->
    Origin = <<16#30, 16#7E, 16#08, 16#7D, 16#55, 16#7E>>,
    Escaped = <<16#30, 16#7D, 16#02, 16#08, 16#7D, 16#01, 16#55, 16#7D, 16#02>>,
    ?assertEqual(escape(Origin, ?FB_7E), Escaped),
    ?assertEqual(unescape(Escaped, ?FB_7E), Origin).

test_escape_3e(_) ->
    Origin = <<16#30, 16#3E, 16#08, 16#3D, 16#55, 16#3E>>,
    Escaped = <<16#30, 16#3D, 16#02, 16#08, 16#3D, 16#01, 16#55, 16#3D, 16#02>>,
    ?assertEqual(escape(Origin, ?FB_3E), Escaped),
    ?assertEqual(unescape(Escaped, ?FB_3E), Origin).
