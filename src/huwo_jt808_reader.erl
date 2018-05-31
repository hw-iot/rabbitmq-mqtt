-module(huwo_jt808_reader).

-export([process_received_bytes/2]).
-export([parse/2]).

process_received_bytes(Bytes, State) ->
    ParseResult = parse(Bytes, State),
    %% case parse(jt808, Bytes, ParseState) of
    %%     {ok, Frame808}
    bin_utils:dump(parse_result, ParseResult),
    ok.

%%----------------------------------------------------------------------------
parse(Bytes, ParseState) ->
    try
        huwo_jt808_frame:parse(Bytes, ParseState)
    catch
        _:Reason ->
            {error, {cannot_parse, Reason, erlang:get_stacktrace()}}
    end.
