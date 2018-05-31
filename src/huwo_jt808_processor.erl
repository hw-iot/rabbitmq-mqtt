-module(huwo_jt808_processor).

-export([process_frame/2]).

process_frame(Frame, PState) ->
    Type = 1,
    process_request(Type, Frame, PState),
    ok.

process_request(_Type, Frame, _PState) ->
    bin_utils:dump(process_request, Frame).
