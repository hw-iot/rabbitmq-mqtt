-module(huwo_jt808_reader).

-export([process_received_bytes/2]).
-export([parse/2]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("rabbit_mqtt.hrl").

process_received_bytes(Bytes,
                       State = #state{ parse_state = ParseState,
                                       proc_state  = ProcState }) ->
    bin_utils:dump(process_received_bytes_state, State),
    case parse(Bytes, ParseState) of
        {ok, Frame}->
            bin_utils:dump(parse_result, Frame),
            case huwo_jt808_processor:process_frame(Frame, ProcState) of
                {ok} ->
                    {ok}
            end;
        {error, Error} ->
            rabbit_log_connection:error("JT808 detected framing error '~p'~n",
                                        [Error]),
            {stop, {shutdown, Error}, State}
    end.

%%----------------------------------------------------------------------------
parse(Bytes, ParseState) ->
    try
        huwo_jt808_frame:parse(Bytes, ParseState)
    catch
        _:Reason ->
            {error, {cannot_parse, Reason, erlang:get_stacktrace()}}
    end.
