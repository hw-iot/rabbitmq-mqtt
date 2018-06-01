%% gen_server work module

-module(huwo_jt808_reader).
-behaviour(gen_server2).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([conserve_resources/3, start_keepalive/2]).

-export([info/2]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("rabbit_mqtt.hrl").


% used for debug
-export([process_received_bytes/2, parse/2]).


start_link(KeepaliveSup, Ref, Sock) ->
    Pid = proc_lib:spawn_link(?MODULE, init,
                              [[KeepaliveSup, Ref, Sock]]),

    %% In the event that somebody floods us with connections, the
    %% reader processes can spew log events at error_logger faster
    %% than it can keep up, causing its mailbox to grow unbounded
    %% until we eat all the memory available and crash. So here is a
    %% meaningless synchronous call to the underlying gen_event
    %% mechanism. When it returns the mailbox is drained, and we
    %% return to our caller to accept more connections.
    gen_event:which_handlers(error_logger),

    {ok, Pid}.

conserve_resources(Pid, _, {_, Conserve, _}) ->
    Pid ! {conserve_resources, Conserve},
    ok.

% keepalive
start_keepalive(_,   0        ) -> ok;
start_keepalive(Pid, Keepalive) -> Pid ! {start_keepalives, Keepalive}.

info(Pid, InfoItems) ->
    case InfoItems -- ?INFO_ITEMS of
        [] -> gen_server2:call(Pid, {info, InfoItems});
        UnknownItems -> throw({bad_argument, UnknownItems})
    end.
%%----------------------------------------------------------------------------

process_received_bytes(Bytes, State) ->
    case parse(Bytes, State) of
        {ok, Frame}->
            bin_utils:dump(parse_result, Frame),
            case huwo_jt808_processor:process_frame(Frame, State) of
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
