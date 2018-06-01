%% gen_server work module

-module(huwo_jt808_reader).
-behaviour(gen_server2).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([conserve_resources/3, start_keepalive/2]).

-export([info/2]).


-include_lib("amqp_client/include/amqp_client.hrl").
-include("rabbit_mqtt.hrl"). % 暂时先用mqtt的头部


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

%% gen_server callback

init([KeepaliveSup, Ref, Sock]) ->
    process_flag(trap_exit, true), % ??? 啥函数
    RealSocket = rabbit_net:unwrap_socket(Sock),
    rabbit_networking:accept_ack(Ref, RealSocket),
    case rabbit_net:connection_string(Sock, inbound) of
        {ok, ConnStr} ->
            rabbit_log_connection:debug("Huwo JT808 accepting TCP connection ~p (~s)~n", [self(), ConnStr]),
            rabbit_alarm:register(
              self(), {?MODULE, conserve_resources, []}),
            ProcessorState = rabbit_mqtt_processor:initial_state(Sock,ssl_login_name(RealSocket)),
            gen_server2:enter_loop(?MODULE, [],
             rabbit_event:init_stats_timer(
              control_throttle(
               #state{socket                 = RealSocket,
                      conn_name              = ConnStr,
                      await_recv             = false,
                      connection_state       = running,
                      received_connect_frame = false,
                      keepalive              = {none, none},
                      keepalive_sup          = KeepaliveSup,
                      conserve               = false,
                      parse_state            = rabbit_mqtt_frame:initial_state(),
                      proc_state             = ProcessorState }), #state.stats_timer),
             {backoff, 1000, 1000, 10000});
        {network_error, Reason} ->
            rabbit_net:fast_close(RealSocket),
            terminate({shutdown, Reason}, undefined);
        {error, enotconn} ->
            rabbit_net:fast_close(RealSocket),
            terminate(shutdown, undefined);
        {error, Reason} ->
            rabbit_net:fast_close(RealSocket),
            terminate({network_error, Reason}, undefined)
    end.
do_terminate({network_error, {ssl_upgrade_error, closed}, ConnStr}, _State) ->
    rabbit_log_connection:error("MQTT detected TLS upgrade error on ~s: connection closed~n",
       [ConnStr]);

do_terminate({network_error,
           {ssl_upgrade_error,
            {tls_alert, "handshake failure"}}, ConnStr}, _State) ->
    rabbit_log_connection:error("MQTT detected TLS upgrade error on ~s: handshake failure~n",
       [ConnStr]);

do_terminate({network_error,
           {ssl_upgrade_error,
            {tls_alert, "unknown ca"}}, ConnStr}, _State) ->
    rabbit_log_connection:error("MQTT detected TLS certificate verification error on ~s: alert 'unknown CA'~n",
       [ConnStr]);

do_terminate({network_error,
           {ssl_upgrade_error,
            {tls_alert, Alert}}, ConnStr}, _State) ->
    rabbit_log_connection:error("MQTT detected TLS upgrade error on ~s: alert ~s~n",
       [ConnStr, Alert]);

do_terminate({network_error, {ssl_upgrade_error, Reason}, ConnStr}, _State) ->
    rabbit_log_connection:error("MQTT detected TLS upgrade error on ~s: ~p~n",
        [ConnStr, Reason]);

do_terminate({network_error, Reason, ConnStr}, _State) ->
    rabbit_log_connection:error("MQTT detected network error on ~s: ~p~n",
        [ConnStr, Reason]);

do_terminate({network_error, Reason}, _State) ->
    rabbit_log_connection:error("MQTT detected network error: ~p~n", [Reason]);

do_terminate(normal, #state{proc_state = ProcState,
                         conn_name  = ConnName}) ->
    rabbit_mqtt_processor:close_connection(ProcState),
    rabbit_log_connection:info("closing MQTT connection ~p (~s)~n", [self(), ConnName]),
    ok;

do_terminate(_Reason, #state{proc_state = ProcState}) ->
    rabbit_mqtt_processor:close_connection(ProcState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

ssl_login_name(Sock) ->
  case rabbit_net:peercert(Sock) of
      {ok, C}              -> case rabbit_ssl:peer_cert_auth_name(C) of
                                    unsafe    -> none;
                                    not_found -> none;
                                    Name      -> Name
                                end;
      {error, no_peercert} -> none;
      nossl                -> none
  end.

%%----------------------------------------------------------------------------

% biz

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




%%----------------------------------------------------------------------------
% private

control_throttle(State = #state{ connection_state = Flow,
                                 conserve         = Conserve }) ->
    case {Flow, Conserve orelse credit_flow:blocked()} of
        {running,   true} -> ok = rabbit_heartbeat:pause_monitor(
                                    State#state.keepalive),
                             State #state{ connection_state = blocked };
        {blocked,  false} -> ok = rabbit_heartbeat:resume_monitor(
                                    State#state.keepalive),
                             run_socket(State #state{
                                                connection_state = running });
        {_,            _} -> run_socket(State)
    end.
