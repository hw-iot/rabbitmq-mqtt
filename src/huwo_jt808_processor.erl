-module(huwo_jt808_processor).

-export([info/2, initial_state/2, initial_state/4,
         process_frame/2, amqp_pub/2, amqp_callback/2, send_will/1,
         close_connection/1]).

%% for testing purposes
-export([get_vhost_username/1, get_vhost/3, get_vhost_from_user_mapping/2]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("rabbit_mqtt.hrl").
-include("huwo_jt808.hrl").
-include("huwo_jt808_frame.hrl").

-define(APP, rabbitmq_jt808).
%% ?FRAME_TYPE ~= ?FRAME
%% -define(FRAME_TYPE(Frame, Type),
%%         Frame = #mqtt_frame{ fixed = #mqtt_frame_fixed{ type = Type }}).

%% call(#Port<rabbit@morgana.18500>, none)
%% returned:
%% #proc_state{
%%    socket = #Port<rabbit@morgana.18500>,
%%    subscriptions = #{},
%%    consumer_tags = {undefined, undefined},
%%    unacked_pubs = {0,nil},
%%    awaiting_ack = {0,nil},
%%    awaiting_seqno = undefined,
%%    message_id = 1,
%%    client_id = undefined,
%%    clean_sess = undefined,
%%    will_msg = undefined,
%%    channels = {undefined, undefined},
%%    connection = undefined,
%%    exchange = <<"amq.topic">>,
%%    adapter_info = #amqp_adapter_info{
%%       host = {0,0,0,0,0,0,0,1},
%%       port = 1883,
%%       peer_host = {0,0,0,0,0,0,0,1},
%%       peer_port = 60266,
%%       name = <<"[::1]:60266 -> [::1]:1883">>,
%%       protocol = {'JT808', "N/A"},
%%       additional_info = [{channels, 1},
%%                          {channel_max, 1},
%%                          {frame_max, 0},
%%                          {client_properties,
%%                           [{<<"product">>, longstr, <<"JT808 client">>}]},
%%                          {ssl, false}]},
%%    ssl_login_name = none,
%%    retainer_pid = undefined,
%%    auth_state = undefined,
%%    send_fun #Fun<rabbit_mqtt_processor.0.11017165>}
initial_state(Socket, SSLLoginName) ->
    RealSocket = rabbit_net:unwrap_socket(Socket),
    initial_state(RealSocket, SSLLoginName,
                  adapter_info(Socket, 'JT808'),
                  fun send_client/2).

initial_state(Socket, SSLLoginName,
              AdapterInfo0 = #amqp_adapter_info{additional_info = Extra},
              SendFun) ->
    %% JT808 connections use exactly one channel. The frame max is not
    %% applicable and there is no way to know what client is used.
    AdapterInfo = AdapterInfo0#amqp_adapter_info{
                    additional_info = [{channels, 1},
                                       {channel_max, 1},
                                       {frame_max, 0},
                                       {client_properties,
                                        [{<<"product">>, longstr, <<"JT808 client">>}]} | Extra]},
    #proc_state{ unacked_pubs   = gb_trees:empty(),
                 awaiting_ack   = gb_trees:empty(),
                 message_id     = 1,
                 subscriptions  = #{},
                 consumer_tags  = {undefined, undefined},
                 channels       = {undefined, undefined},
                 exchange       = rabbit_mqtt_util:env(exchange),
                 socket         = Socket,
                 adapter_info   = AdapterInfo,
                 ssl_login_name = SSLLoginName,
                 send_fun       = SendFun }.

%%-----------------------------------------
%% 开始处理包
%% 消息头已解析，可以取得消息类型MsgID。消息体 Payload 为二进制，待进一步解析
%% 如果不是注册设备的消息，但状态中的connection没定义说明没有注册就发送其他信息
process_frame(#huwo_jt808_frame{
                 header = #huwo_jt808_frame_header{ id = Type }},
              PState = #proc_state{ connection = undefined })
  when Type =/= ?CONNECT ->
    {error, connect_expected, PState};
%% call(_, PState = initial_state:retrun:#proc_state)
process_frame(#huwo_jt808_frame{
                 header = #huwo_jt808_frame_header{
                             id = Type}} = Frame, PState) ->
    ?DEBUG(processor_process_frame_frame, Frame),
    case process_request(Type, Frame, PState) of
        {ok, PState1} -> {ok, PState1, PState1#proc_state.connection};
        Ret -> Ret
    end.

%% call(?CONNECT, _, PState = initial_state:retrun:#proc_state)
%% {huwo_jt808_frame,
%%  {huwo_jt808_frame_header,259,0,0,0,77,201805141800,12},
%%  {huwo_jt808_frame_connect,undefined,<<"13896079527">>,
%%   <<"huwo-erlang-jt808-client">>,<<"user">>,<<"pass">>,0,
%%   <<"iPhone 3G">>,<<"2.1.1OSX 10">>,<<"2.1.1OSX 10">>,1}}
process_request(?CONNECT,
                #huwo_jt808_frame{
                   payload = #huwo_jt808_frame_connect{
                                client_id = ClientId0,
                                clean_sess = CleanSess,
                                keep_alive = Keepalive,

                                mobile = _Mobile,
                                client_name = _ClientName,
                                username = Username,
                                password = Password,
                                client_type = _ClientType,
                                phone_model = _PhoneModel,
                                proto_ver = ProtoVersion,
                                phone_os = _ProtoOS,
                                work_mode = _WorkMode} = Var} = Request,
                PState0 = #proc_state{ ssl_login_name = SSLLoginName,
                                       send_fun       = SendFun,
                                       adapter_info   = AdapterInfo = #amqp_adapter_info{additional_info = Extra} }) ->
    %% ClientId = "013896079527" | "IYZ-hf2cvlQdS1IqCWTmqA"
    ClientId = case ClientId0 of
                   []    -> rabbit_mqtt_util:gen_client_id();
                   [_|_] -> ClientId0
               end,
    AdapterInfo1 = AdapterInfo#amqp_adapter_info{
                     additional_info =
                         [{variable_map, #{<<"client_id">> => rabbit_data_coercion:to_binary(ClientId)}} | Extra]},
    PState = PState0#proc_state{adapter_info = AdapterInfo1},
    {Return, PState1} =
        case {lists:member(ProtoVersion, proplists:get_keys(?PROTOCOL_NAMES)),
              ClientId0 =:= [] andalso CleanSess =:= false} of
            {false, _} ->
                {?CONNACK_PROTO_VER, PState};
            {_, true} ->
                {?CONNACK_INVALID_ID, PState};
            _ ->
                %% call ("guest","guest",none)
                %% returned {<<"guest">>, <<"guest">>}
                case creds(Username, Password, SSLLoginName) of
                    nocreds ->
                        rabbit_log_connection:error("JT808 login failed: no credentials provided~n"),
                        {?CONNACK_CREDENTIALS, PState};
                    {invalid_creds, {undefined, Pass}} when is_list(Pass) ->
                        rabbit_log_connection:error("JT808 login failed: no user username is provided"),
                        {?CONNACK_CREDENTIALS, PState};
                    {invalid_creds, {User, undefined}} when is_list(User) ->
                        rabbit_log_connection:error("JT808 login failed for ~p: no password provided", [User]),
                        {?CONNACK_CREDENTIALS, PState};
                    {UserBin, PassBin} ->
                        case process_login(UserBin, PassBin, ProtoVersion, PState) of
                            {?CONNACK_ACCEPT, Conn, VHost, AState} ->
                                RetainerPid =
                                    rabbit_mqtt_retainer_sup:child_for_vhost(VHost),
                                link(Conn),
                                {ok, Ch} = amqp_connection:open_channel(Conn),
                                link(Ch),
                                amqp_channel:enable_delivery_flow_control(Ch),
                                ok = huwo_jt808_collector:register(
                                       ClientId, self()),
                                Prefetch = rabbit_mqtt_util:env(prefetch),
                                #'basic.qos_ok'{} = amqp_channel:call(
                                                      Ch, #'basic.qos'{prefetch_count = Prefetch}),
                                huwo_jt808_reader:start_keepalive(self(), Keepalive),
                                {SP, ProcState} =
                                    maybe_clean_sess(
                                      PState #proc_state{
                                        will_msg   = make_will_msg(Var),
                                        clean_sess = CleanSess,
                                        channels   = {Ch, undefined},
                                        connection = Conn,
                                        client_id  = ClientId,
                                        retainer_pid = RetainerPid,
                                        auth_state = AState}),
                                {{?CONNACK_ACCEPT, SP}, ProcState};
                            ConnAck ->
                                {ConnAck, PState}
                        end
                end
        end,
    %% {0,false}
    {ReturnCode, _SessionPresent} = case Return of
                                        {?CONNACK_ACCEPT, _} = Return -> Return;
                                        Return                        -> {Return, false}
                                    end,
    %% SendFun(#mqtt_frame{ fixed    = #mqtt_frame_fixed{ type = ?CONNACK},
    %%                      variable = #mqtt_frame_connack{
    %%                                    session_present = SessionPresent,
    %%                                    return_code = ReturnCode}},
    %%         PState1),
    SendFun(huwo_jt808_session:response(Request, ReturnCode), PState1),
    Msg = #huwo_jt808_msg{ qos = ?QOS_0, payload = <<"bingo">>, topic = <<"topic">>},
    bin_utils:dump(process_request_publish_amqp_pub, Msg),
    amqp_pub(Msg, PState1),
    %% TODO hw-iot ----------------
    process_subscribe([#huwo_topic{name = "topic", qos=2}], PState1),
    {ok, PState1};
process_request(_AnyType, #huwo_jt808_frame{payload = Payload}, PState) ->
    Msg = #huwo_jt808_msg{ qos = ?QOS_0, payload = Payload, topic = <<"topic">>},
    amqp_pub(Msg, PState),
    {ok, PState}.

%%---------------------------------------------------------------------
hand_off_to_retainer(RetainerPid, Topic, #huwo_jt808_msg{payload = <<"">>}) ->
    %% TODO: retainer支持
    rabbit_mqtt_retainer:clear(RetainerPid, Topic),
    ok;
hand_off_to_retainer(RetainerPid, Topic, Msg) ->
    %% TODO: retainer支持
    rabbit_mqtt_retainer:retain(RetainerPid, Topic, Msg),
    ok.

maybe_send_retained_message(RPid, #huwo_topic{name = S, qos = SubscribeQos}, MsgId,
                            #proc_state{ send_fun = SendFun } = PState) ->
    case rabbit_mqtt_retainer:fetch(RPid, S) of
        undefined -> false;
        Msg       ->
            %% calculate effective QoS as the lower value of SUBSCRIBE frame QoS
            %% and retained message QoS. The spec isn't super clear on this, we
            %% do what Mosquitto does, per user feedback.
            Qos = erlang:min(SubscribeQos, Msg#mqtt_msg.qos),
            Id = case Qos of
                     ?QOS_0 -> undefined;
                     ?QOS_1 -> MsgId
                 end,
            SendFun(#mqtt_frame{fixed = #mqtt_frame_fixed{
                                           type = 1,
                                           qos  = Qos,
                                           dup  = false,
                                           retain = Msg#mqtt_msg.retain
                                          }, variable = #mqtt_frame_publish{
                                                           message_id = Id,
                                                           topic_name = S
                                                          },
                                payload = Msg#mqtt_msg.payload}, PState),
            case Qos of
                ?QOS_0 -> false;
                ?QOS_1 -> {true, 1}
            end
    end.

%% amqp_callback
%% TODO: 从mqtt frame 提取的，mqtt的消息类型，换为JT808后要去掉
-define(PUBLISH,      3).
-define(PUBACK,       4).
%% -record('basic.deliver', {consumer_tag, delivery_tag, redelivered = false, exchange, routing_key}).
%% -record(amqp_msg, {props = #'P_basic'{}, payload = <<>>}).
amqp_callback({#'basic.deliver'{ consumer_tag = ConsumerTag,
                                 delivery_tag = DeliveryTag,
                                 routing_key  = RoutingKey },
               #amqp_msg{ props = #'P_basic'{ headers = Headers },
                          payload = Payload },
               DeliveryCtx} = Delivery,
              #proc_state{ channels      = {Channel, _},
                           awaiting_ack  = Awaiting,
                           message_id    = MsgId,
                           send_fun      = SendFun } = PState) ->
    amqp_channel:notify_received(DeliveryCtx),
    case {delivery_dup(Delivery), delivery_qos(ConsumerTag, Headers, PState)} of
        {true, {?QOS_0, ?QOS_1}} ->
            amqp_channel:cast(
              Channel, #'basic.ack'{ delivery_tag = DeliveryTag }),
            {ok, PState};
        {true, {?QOS_0, ?QOS_0}} ->
            {ok, PState};
        {Dup, {DeliveryQos, _SubQos} = Qos}     ->
            _ = {Dup, RoutingKey, DeliveryQos},
            SendFun(
              %% TODO: 替换为JT808报文
              %% #mqtt_frame{ fixed = #mqtt_frame_fixed{
              %%                         type = ?PUBLISH,
              %%                         qos  = DeliveryQos,
              %%                         dup  = Dup },
              %%              variable = #mqtt_frame_publish{
              %%                            message_id =
              %%                                case DeliveryQos of
              %%                                    ?QOS_0 -> undefined;
              %%                                    ?QOS_1 -> MsgId
              %%                                end,
              %%                            topic_name =
              %%                                %% TODO: 替换为JT808
              %%                                rabbit_mqtt_util:amqp2mqtt(
              %%                                  RoutingKey) },
              %%              payload = Payload},
              Payload,
              PState),
            case Qos of
                {?QOS_0, ?QOS_0} ->
                    {ok, PState};
                {?QOS_1, ?QOS_1} ->
                    Awaiting1 = gb_trees:insert(MsgId, DeliveryTag, Awaiting),
                    PState1 = PState#proc_state{ awaiting_ack = Awaiting1 },
                    PState2 = next_msg_id(PState1),
                    {ok, PState2};
                {?QOS_0, ?QOS_1} ->
                    amqp_channel:cast(
                      Channel, #'basic.ack'{ delivery_tag = DeliveryTag }),
                    {ok, PState}
            end
    end;

amqp_callback(#'basic.ack'{ multiple = true, delivery_tag = Tag } = Ack,
              PState = #proc_state{ unacked_pubs = UnackedPubs,
                                    send_fun     = SendFun }) ->
    case gb_trees:size(UnackedPubs) > 0 andalso
        gb_trees:take_smallest(UnackedPubs) of
        {TagSmall, MsgId, UnackedPubs1} when TagSmall =< Tag ->
            SendFun(
              %% TODO: 替换为JT808报文
              #mqtt_frame{ fixed    = #mqtt_frame_fixed{ type = ?PUBACK },
                           variable = #mqtt_frame_publish{ message_id = MsgId }},
              PState),
            amqp_callback(Ack, PState #proc_state{ unacked_pubs = UnackedPubs1 });
        _ ->
            {ok, PState}
    end;

amqp_callback(#'basic.ack'{ multiple = false, delivery_tag = Tag },
              PState = #proc_state{ unacked_pubs = UnackedPubs,
                                    send_fun     = SendFun }) ->
    SendFun(
      %% TODO: 替换为JT808报文
      #mqtt_frame{ fixed    = #mqtt_frame_fixed{ type = ?PUBACK },
                   variable = #mqtt_frame_publish{
                                 message_id = gb_trees:get(
                                                Tag, UnackedPubs) }}, PState),
    {ok, PState #proc_state{ unacked_pubs = gb_trees:delete(Tag, UnackedPubs) }}.

delivery_dup({#'basic.deliver'{ redelivered = Redelivered },
              #amqp_msg{ props = #'P_basic'{ headers = Headers }},
              _DeliveryCtx}) ->
    %% TODO: 替换为JT808
    case rabbit_mqtt_util:table_lookup(Headers, <<"x-mqtt-dup">>) of
        undefined   -> Redelivered;
        {bool, Dup} -> Redelivered orelse Dup
    end.

ensure_valid_mqtt_message_id(Id) when Id >= 16#ffff ->
    1;
ensure_valid_mqtt_message_id(Id) ->
    Id.

safe_max_id(Id0, Id1) ->
    ensure_valid_mqtt_message_id(erlang:max(Id0, Id1)).

next_msg_id(PState = #proc_state{ message_id = MsgId0 }) ->
    MsgId1 = ensure_valid_mqtt_message_id(MsgId0 + 1),
    PState#proc_state{ message_id = MsgId1 }.

%% decide at which qos level to deliver based on subscription
%% and the message publish qos level. non-MQTT publishes are
%% assumed to be qos 1, regardless of delivery_mode.
%% {function_clause,[{huwo_jt808_processor,delivery_qos,[<<"amq.ctag-0BCwf6nw9UhcaOLtVgadSg">>,undefined,{proc_state,#Port<0.13271>,#{},{undefined,undefined},{0,nil},{0,nil},undefined,1,undefined,undefined,undefined,{undefined,undefined},undefined,<<"amq.topic">>,{amqp_adapter_info,{0,0,0,0,0,0,0,1},1883,{0,0,0,0,0,0,0,1},50119,<<"[::1]:50119 -> [::1]:1883">>,{'JT808',"N/A"},[{channels,1},{channel_max,1},{frame_max,0},{client_properties,[{<<"product">>,longstr,<<"JT808 client">>}]},{ssl,false}]},none,undefined,undefined,#Fun<huwo_jt808_processor.0.12800453>}],[{file,"src/huwo_jt808_processor.erl"},{line,544}]}
delivery_qos(_, _, _) ->
    {?QOS_1, ?QOS_1}.

%% delivery_qos(Tag, _Headers,  #proc_state{ consumer_tags = {Tag, _} }) ->
%%     {?QOS_0, ?QOS_0};
%% delivery_qos(Tag, _Headers,   #proc_state{ consumer_tags = {_, Tag} }) ->
%%     {?QOS_1, ?QOS_1}.
%% case rabbit_mqtt_util:table_lookup(Headers, <<"x-mqtt-publish-qos">>) of
%%     {byte, Qos} -> {lists:min([Qos, ?QOS_1]), ?QOS_1};
%%     undefined   -> {?QOS_1, ?QOS_1}
%% end.

maybe_clean_sess(PState = #proc_state { clean_sess = false,
                                        channels   = {Channel, _},
                                        client_id  = ClientId }) ->
    {_Queue, PState1} = ensure_queue(?QOS_1, PState),
    SessionPresent = session_present(Channel, ClientId),
    {SessionPresent, PState1};
maybe_clean_sess(PState = #proc_state { clean_sess = true,
                                        connection = Conn,
                                        client_id  = ClientId }) ->
    {_, Queue} = rabbit_mqtt_util:subcription_queue_name(ClientId),
    {ok, Channel} = amqp_connection:open_channel(Conn),
    try amqp_channel:call(Channel, #'queue.delete'{ queue = Queue }) of
        #'queue.delete_ok'{} -> ok = amqp_channel:close(Channel)
    catch
        exit:_Error -> ok
    end,
    {false, PState}.

session_present(Channel, ClientId)  ->
    {_, QueueQ1} = rabbit_mqtt_util:subcription_queue_name(ClientId),
    Declare = #'queue.declare'{queue   = QueueQ1,
                               passive = true},
    case amqp_channel:call(Channel, Declare) of
        #'queue.declare_ok'{} -> true;
        _                     -> false
    end.

%% call(#mqtt_frame_connect{proto_ver = 3,
%%                          will_retain = false,
%%                          will_qos = 0,
%%                          will_flag = false,
%%                          clean_sess = true,
%%                          keep_alive = 60,
%%                          client_id = "mosqsub|73961-morgana",
%%                          will_topic = undefined,
%%                          will_msg = undefined,
%%                          username = undefined,
%%                          password = undefined}).
make_will_msg(#huwo_jt808_frame_connect{ will_flag   = false }) ->
    undefined;
make_will_msg(#huwo_jt808_frame_connect{ will_retain = Retain,
                                         will_qos    = Qos,
                                         will_topic  = Topic,
                                         will_msg    = Msg }) ->
    %% TODO
    #mqtt_msg{ retain  = Retain,
               qos     = Qos,
               topic   = Topic,
               dup     = false,
               payload = Msg }.

process_login(UserBin, PassBin, ProtoVersion,
              #proc_state{ channels     = {undefined, undefined},
                           socket       = Sock,
                           adapter_info = AdapterInfo,
                           ssl_login_name = SslLoginName}) ->
    {ok, {_, _, _, ToPort}} = rabbit_net:socket_ends(Sock, inbound),
    %% call get_vhost(<<"guest">>,none,1883)
    %% returned {default_vhost, {<<"/">>, <<"guest">>}}
    {VHostPickedUsing, {VHost, UsernameBin}} = get_vhost(UserBin, SslLoginName, ToPort),
    rabbit_log_connection:info(
      "JT808 vhost picked using ~s~n",
      [human_readable_vhost_lookup_strategy(VHostPickedUsing)]),
    case rabbit_vhost:exists(VHost) of
        true  ->
            case amqp_connection:start(#amqp_params_direct{
                                          username     = UsernameBin,
                                          password     = PassBin,
                                          virtual_host = VHost,
                                          %% call (_, 3)
                                          %% returned #amqp_adapter_info.protocol = {'JT808', "201.1.1-huwo"}
                                          adapter_info = set_proto_version(AdapterInfo, ProtoVersion)}) of
                {ok, Connection} ->
                    case rabbit_access_control:check_user_loopback(UsernameBin, Sock) of
                        ok ->
                            [{internal_user, InternalUser}] = amqp_connection:info(
                                                                Connection, [internal_user]),
                            {?CONNACK_ACCEPT, Connection, VHost,
                             %% (<0.455.0>) returned process_login/4 -> {0,
                             %%     <0.458.0>,
                             %%     <<"/">>,
                             %%     {auth_state,
                             %%      <<"guest">>,
                             %%      {user,
                             %%       <<"guest">>,
                             %%       [administrator],
                             %%       [{rabbit_auth_backend_internal,
                             %%         none}]},
                             %%      <<"/">>}}
                             #auth_state{user = InternalUser,
                                         username = UsernameBin,
                                         vhost = VHost}};
                        not_allowed ->
                            amqp_connection:close(Connection),
                            rabbit_log_connection:warning(
                              "JT808 login failed for ~p access_refused "
                              "(access must be from localhost)~n",
                              [binary_to_list(UsernameBin)]),
                            ?CONNACK_AUTH
                    end;
                {error, {auth_failure, Explanation}} ->
                    rabbit_log_connection:error("JT808 login failed for ~p auth_failure: ~s~n",
                                                [binary_to_list(UserBin), Explanation]),
                    ?CONNACK_CREDENTIALS;
                {error, access_refused} ->
                    rabbit_log_connection:warning("JT808 login failed for ~p access_refused "
                                                  "(vhost access not allowed)~n",
                                                  [binary_to_list(UserBin)]),
                    ?CONNACK_AUTH;
                {error, not_allowed} ->
                    %% when vhost allowed for TLS connection
                    rabbit_log_connection:warning("JT808 login failed for ~p access_refused "
                                                  "(vhost access not allowed)~n",
                                                  [binary_to_list(UserBin)]),
                    ?CONNACK_AUTH
            end;
        false ->
            rabbit_log_connection:error("JT808 login failed for ~p auth_failure: vhost ~s does not exist~n",
                                        [binary_to_list(UserBin), VHost]),
            ?CONNACK_CREDENTIALS
    end.

get_vhost(UserBin, none, Port) ->
    %% call get_vhost_no_ssl(<<"guest">>,1883)
    %% returned {default_vhost, {<<"/">>, <<"guest">>}}
    get_vhost_no_ssl(UserBin, Port);
get_vhost(UserBin, undefined, Port) ->
    get_vhost_no_ssl(UserBin, Port);
get_vhost(UserBin, SslLogin, Port) ->
    get_vhost_ssl(UserBin, SslLogin, Port).

get_vhost_no_ssl(UserBin, Port) ->
    case vhost_in_username(UserBin) of
        true  ->
            {vhost_in_username_or_default, get_vhost_username(UserBin)};
        false ->
            PortVirtualHostMapping = rabbit_runtime_parameters:value_global(
                                       mqtt_port_to_vhost_mapping  % TODO
                                      ),
            %% call get_vhost_from_port_mapping(1883,not_found)
            %% returned get_vhost_from_port_mapping/2 -> undefined
            case get_vhost_from_port_mapping(Port, PortVirtualHostMapping) of
                undefined ->
                    {default_vhost, {rabbit_mqtt_util:env(vhost), UserBin}};
                VHost ->
                    {port_to_vhost_mapping, {VHost, UserBin}}
            end
    end.

get_vhost_ssl(UserBin, SslLoginName, Port) ->
    UserVirtualHostMapping = rabbit_runtime_parameters:value_global(
                               mqtt_default_vhosts
                              ),
    case get_vhost_from_user_mapping(SslLoginName, UserVirtualHostMapping) of
        undefined ->
            PortVirtualHostMapping = rabbit_runtime_parameters:value_global(
                                       mqtt_port_to_vhost_mapping
                                      ),
            case get_vhost_from_port_mapping(Port, PortVirtualHostMapping) of
                undefined ->
                    {vhost_in_username_or_default, get_vhost_username(UserBin)};
                VHostFromPortMapping ->
                    {port_to_vhost_mapping, {VHostFromPortMapping, UserBin}}
            end;
        VHostFromCertMapping ->
            {cert_to_vhost_mapping, {VHostFromCertMapping, UserBin}}
    end.

vhost_in_username(UserBin) ->
    case application:get_env(?APP, ignore_colons_in_username) of
        {ok, true} -> false;
        _ ->
            %% split at the last colon, disallowing colons in username
            case re:split(UserBin, ":(?!.*?:)") of
                [_, _]      -> true;
                [UserBin]   -> false
            end
    end.

get_vhost_username(UserBin) ->
    Default = {rabbit_mqtt_util:env(vhost), UserBin},
    case application:get_env(?APP, ignore_colons_in_username) of
        {ok, true} -> Default;
        _ ->
            %% split at the last colon, disallowing colons in username
            case re:split(UserBin, ":(?!.*?:)") of
                [Vhost, UserName] -> {Vhost,  UserName};
                [UserBin]         -> Default
            end
    end.

get_vhost_from_user_mapping(_User, not_found) ->
    undefined;
get_vhost_from_user_mapping(User, Mapping) ->
    M = rabbit_data_coercion:to_proplist(Mapping),
    case rabbit_misc:pget(User, M) of
        undefined ->
            undefined;
        VHost ->
            VHost
    end.

get_vhost_from_port_mapping(_Port, not_found) ->
    undefined;
get_vhost_from_port_mapping(Port, Mapping) ->
    M = rabbit_data_coercion:to_proplist(Mapping),
    Res = case rabbit_misc:pget(rabbit_data_coercion:to_binary(Port), M) of
              undefined ->
                  undefined;
              VHost ->
                  VHost
          end,
    Res.

human_readable_vhost_lookup_strategy(vhost_in_username_or_default) ->
    "vhost in username or default";
human_readable_vhost_lookup_strategy(port_to_vhost_mapping) ->
    "MQTT port to vhost mapping";
human_readable_vhost_lookup_strategy(cert_to_vhost_mapping) ->
    "client certificate to vhost mapping";
human_readable_vhost_lookup_strategy(default_vhost) ->
    "plugin configuration or default";
human_readable_vhost_lookup_strategy(Val) ->
    atom_to_list(Val).

creds(User, Pass, SSLLoginName) ->
    DefaultUser   = rabbit_mqtt_util:env(default_user),
    DefaultPass   = rabbit_mqtt_util:env(default_pass),
    %% {DefaultUser, DefaultPass}.
    %% TODO
    {ok, Anon}    = application:get_env(?APP, allow_anonymous),
    {ok, TLSAuth} = application:get_env(?APP, ssl_cert_login),
    HaveDefaultCreds = Anon =:= true andalso
        is_binary(DefaultUser) andalso
        is_binary(DefaultPass),

    CredentialsProvided = User =/= undefined orelse
        Pass =/= undefined,

    CorrectCredentials = is_list(User) andalso
        is_list(Pass),

    SSLLoginProvided = TLSAuth =:= true andalso
        SSLLoginName =/= none,

    case {CredentialsProvided, CorrectCredentials, SSLLoginProvided, HaveDefaultCreds} of
        %% Username and password take priority
        {true, true, _, _}          -> {list_to_binary(User),
                                        list_to_binary(Pass)};
        %% Either username or password is provided
        {true, false, _, _}         -> {invalid_creds, {User, Pass}};
        %% rabbitmq_mqtt.ssl_cert_login is true. SSL user name provided.
        %% Authenticating using username only.
        {false, false, true, _}     -> {SSLLoginName, none};
        %% Anonymous connection uses default credentials
        {false, false, false, true} -> {DefaultUser, DefaultPass};
        _                           -> nocreds
    end.

supported_subs_qos(?QOS_0) -> ?QOS_0;
supported_subs_qos(?QOS_1) -> ?QOS_1;
supported_subs_qos(?QOS_2) -> ?QOS_1.

delivery_mode(?QOS_0) -> 1;
delivery_mode(?QOS_1) -> 2.

%% different qos subscriptions are received in different queues
%% with appropriate durability and timeout arguments
%% this will lead to duplicate messages for overlapping subscriptions
%% with different qos values - todo: prevent duplicates
ensure_queue(Qos, #proc_state{ channels      = {Channel, _},
                               client_id     = ClientId,
                               clean_sess    = CleanSess,
                               consumer_tags = {TagQ0, TagQ1} = Tags} = PState) ->
    {QueueQ0, QueueQ1} = rabbit_mqtt_util:subcription_queue_name(ClientId),
    Qos1Args = case {rabbit_mqtt_util:env(subscription_ttl), CleanSess} of
                   {undefined, _} ->
                       [];
                   {Ms, false} when is_integer(Ms) ->
                       [{<<"x-expires">>, long, Ms}];
                   _ ->
                       []
               end,
    QueueSetup =
        case {TagQ0, TagQ1, Qos} of
            {undefined, _, ?QOS_0} ->
                {QueueQ0,
                 #'queue.declare'{ queue       = QueueQ0,
                                   durable     = false,
                                   auto_delete = true },
                 #'basic.consume'{ queue  = QueueQ0,
                                   no_ack = true }};
            {_, undefined, ?QOS_1} ->
                {QueueQ1,
                 #'queue.declare'{ queue       = QueueQ1,
                                   durable     = true,
                                   %% Clean session means a transient connection,
                                   %% translating into auto-delete.
                                   %%
                                   %% see rabbitmq/rabbitmq-mqtt#37
                                   auto_delete = CleanSess,
                                   arguments   = Qos1Args },
                 #'basic.consume'{ queue  = QueueQ1,
                                   no_ack = false }};
            {_, _, ?QOS_0} ->
                {exists, QueueQ0};
            {_, _, ?QOS_1} ->
                {exists, QueueQ1}
        end,
    case QueueSetup of
        {Queue, Declare, Consume} ->
            #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
            #'basic.consume_ok'{ consumer_tag = Tag } =
                amqp_channel:call(Channel, Consume),
            {Queue, PState #proc_state{ consumer_tags = setelement(Qos+1, Tags, Tag) }};
        {exists, Q} ->
            {Q, PState}
    end.

send_will(PState = #proc_state{will_msg = undefined}) ->
    PState;
%% TODO: huwo_jt808_msg的内容有什么用途需要研究
send_will(PState = #proc_state{will_msg = WillMsg = #huwo_jt808_msg{retain = Retain,
                                                                    topic = Topic},
                               retainer_pid = RPid,
                               channels = {ChQos0, ChQos1}}) ->
    case check_topic_access(Topic, write, PState) of
        ok ->
            amqp_pub(WillMsg, PState),
            case Retain of
                false -> ok;
                true  -> hand_off_to_retainer(RPid, Topic, WillMsg)
            end;
        Error  ->
            rabbit_log:warning(
              "Could not send last will: ~p~n",
              [Error])
    end,
    case ChQos1 of
        undefined -> ok;
        _         -> amqp_channel:close(ChQos1)
    end,
    case ChQos0 of
        undefined -> ok;
        _         -> amqp_channel:close(ChQos0)
    end,
    PState #proc_state{ channels = {undefined, undefined} }.

amqp_pub(undefined, PState) ->
    PState;

%% set up a qos1 publishing channel if necessary
%% this channel will only be used for publishing, not consuming
amqp_pub(Msg    = #huwo_jt808_msg{ qos = ?QOS_1 },
         PState = #proc_state{ channels       = {ChQos0, undefined},
                               awaiting_seqno = undefined,
                               connection     = Conn }) ->
    {ok, Channel} = amqp_connection:open_channel(Conn),
    #'confirm.select_ok'{} = amqp_channel:call(Channel, #'confirm.select'{}),
    amqp_channel:register_confirm_handler(Channel, self()),
    amqp_pub(Msg, PState #proc_state{ channels       = {ChQos0, Channel},
                                      awaiting_seqno = 1 });

amqp_pub(#huwo_jt808_msg{ qos        = Qos,
                          topic      = Topic,
                          dup        = Dup,
                          message_id = MessageId,
                          payload    = Payload },
         PState = #proc_state{ channels       = {ChQos0, ChQos1},
                               exchange       = Exchange,
                               unacked_pubs   = UnackedPubs,
                               awaiting_seqno = SeqNo }) ->
    Method = #'basic.publish'{ exchange    = Exchange,
                               routing_key =
                                   rabbit_mqtt_util:mqtt2amqp(Topic)},
    Headers = [{<<"x-mqtt-publish-qos">>, byte, Qos},
               {<<"x-mqtt-dup">>, bool, Dup}],
    Msg = #amqp_msg{ props   = #'P_basic'{ headers       = Headers,
                                           delivery_mode = delivery_mode(Qos)},
                     payload = Payload },
    {UnackedPubs1, Ch, SeqNo1} =
        case Qos =:= ?QOS_1 andalso MessageId =/= undefined of
            true  -> {gb_trees:enter(SeqNo, MessageId, UnackedPubs), ChQos1,
                      SeqNo + 1};
            false -> {UnackedPubs, ChQos0, SeqNo}
        end,
    amqp_channel:cast_flow(Ch, Method, Msg),
    PState #proc_state{ unacked_pubs   = UnackedPubs1,
                        awaiting_seqno = SeqNo1 }.

adapter_info(Sock, ProtoName) ->
    amqp_connection:socket_adapter_info(Sock, {ProtoName, "N/A"}).


%% Topics = [#huwo_topic{name = "topic", qos=2}],
%% process_request(?SUBSCRIBE,
%%                 #mqtt_frame{
%%                    variable = #mqtt_frame_subscribe{
%%                                  message_id  = SubscribeMsgId,
%%                                  topic_table = Topics},
%%                    payload = undefined},
%%                 #proc_state{channels = {Channel, _},
%%                             exchange = Exchange,
%%                             retainer_pid = RPid,
%%                             send_fun = SendFun,
%%                             message_id  = StateMsgId} = PState0) ->
process_subscribe(Topics,
                  #proc_state{
                     channels = {Channel, _},
                     exchange = Exchange,
                     retainer_pid = RPid,
                     message_id  = StateMsgId} = PState0) ->
    SubscribeMsgId = 1,
    check_subscribe(
      Topics,
      fun() ->
              {_QosResponse, PState1} =
                  lists:foldl(
                    fun (#huwo_topic{name = TopicName, qos  = Qos}, {QosList, PState3}) ->
                            SupportedQos = supported_subs_qos(Qos),
                            {Queue, #proc_state{subscriptions = Subs} = PState1} =
                                ensure_queue(SupportedQos, PState3),
                            Binding = #'queue.bind'{
                                         queue       = Queue,
                                         exchange    = Exchange,
                                         routing_key = rabbit_mqtt_util:mqtt2amqp(
                                                         TopicName)},
                            #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
                            SupportedQosList = case maps:find(TopicName, Subs) of
                                                   {ok, L} -> [SupportedQos|L];
                                                   error   -> [SupportedQos]
                                               end,
                            {[SupportedQos | QosList],
                             PState1 #proc_state{
                               subscriptions =
                                   maps:put(TopicName, SupportedQosList, Subs)}}
                    end, {[], PState0}, Topics),
              %% SendFun(#mqtt_frame{fixed    = #mqtt_frame_fixed{type = ?SUBACK},
              %%                     variable = #mqtt_frame_suback{
              %%                                   message_id = SubscribeMsgId,
              %%                                   qos_table  = QosResponse}}, PState1),

              %% we may need to send up to length(Topics) messages.
              %% if QoS is > 0 then we need to generate a message id,
              %% and increment the counter.
              StartMsgId = safe_max_id(SubscribeMsgId, StateMsgId),
              N = lists:foldl(fun (Topic, Acc) ->
                                      case maybe_send_retained_message(RPid, Topic, Acc, PState1) of
                                          {true, X} -> Acc + X;
                                          false     -> Acc
                                      end
                              end, StartMsgId, Topics),
              {ok, PState1#proc_state{message_id = N}}
      end, PState0).

%%-----------------------------------------------------------------------

set_proto_version(AdapterInfo = #amqp_adapter_info{protocol = {Proto, _}}, Vsn) ->
    AdapterInfo#amqp_adapter_info{protocol = {Proto,
                                              human_readable_mqtt_version(Vsn)}}.

human_readable_mqtt_version(3) ->
    "3.1.0";
human_readable_mqtt_version(4) ->
    "3.1.1";
human_readable_mqtt_version(<<"201.1.1-huwo">>) ->
    "201.1.1-huwo";
human_readable_mqtt_version(_) ->
    "N/A".

send_client(Response, #proc_state{ socket = Sock }) ->
    Frame = huwo_jt808_frame:serialise(Response),
    ?DEBUG(processor_send_client_response, Response),
    ?DEBUG(processor_send_client_frame, Frame),
    rabbit_net:port_command(Sock, Frame).

close_connection(PState = #proc_state{ connection = undefined }) ->
    PState;
close_connection(PState = #proc_state{ connection = Connection,
                                       client_id  = ClientId }) ->
    %% todo: maybe clean session
    case ClientId of
        undefined -> ok;
        _         -> ok = huwo_jt808_collector:unregister(ClientId, self())
    end,
    %% ignore noproc or other exceptions to avoid debris
    catch amqp_connection:close(Connection),
    PState #proc_state{ channels   = {undefined, undefined},
                        connection = undefined }.
%%---------------------------------------------------------------------

%% NB: check_*: MQTT spec says we should ack normally, ie pretend there
%% was no auth error, but here we are closing the connection with an error. This
%% is what happens anyway if there is an authorization failure at the AMQP level.
%% TODO
%% check_publish(TopicName, Fn, PState) ->
%%     case check_topic_access(TopicName, write, PState) of
%%         ok -> Fn();
%%         _ -> {err, unauthorized, PState}
%%     end.

check_subscribe([], Fn, _) ->
    Fn();

check_subscribe([#huwo_topic{name = TopicName} | Topics], Fn, PState) ->
    case check_topic_access(TopicName, read, PState) of
        ok -> check_subscribe(Topics, Fn, PState);
        _ -> {err, unauthorized, PState}
    end.

check_topic_access(TopicName, Access,
                   #proc_state{
                      auth_state = #auth_state{user = User = #user{username = Username},
                                               vhost = VHost},
                      exchange = Exchange,
                      client_id = ClientId}) ->
    Resource = #resource{virtual_host = VHost,
                         kind = topic,
                         name = Exchange},

    Context = #{routing_key  => rabbit_mqtt_util:mqtt2amqp(TopicName),
                variable_map => #{
                                  <<"username">>  => Username,
                                  <<"vhost">>     => VHost,
                                  <<"client_id">> => rabbit_data_coercion:to_binary(ClientId)
                                 }
               },

    try rabbit_access_control:check_topic_access(User, Resource, Access, Context) of
        R -> R
    catch
        _:{amqp_error, access_refused, Msg, _} ->
            rabbit_log:error("operation resulted in an error (access_refused): ~p~n", [Msg]),
            {error, access_refused};
        _:Error ->
            rabbit_log:error("~p~n", [Error]),
            {error, access_refused}
    end.

%% 返回进程状态的指定值
info(consumer_tags, #proc_state{consumer_tags = Val}) -> Val;
info(unacked_pubs, #proc_state{unacked_pubs = Val}) -> Val;
info(awaiting_ack, #proc_state{awaiting_ack = Val}) -> Val;
info(awaiting_seqno, #proc_state{awaiting_seqno = Val}) -> Val;
info(message_id, #proc_state{message_id = Val}) -> Val;
info(client_id, #proc_state{client_id = Val}) ->
    rabbit_data_coercion:to_binary(Val);
info(clean_sess, #proc_state{clean_sess = Val}) -> Val;
info(will_msg, #proc_state{will_msg = Val}) -> Val;
info(channels, #proc_state{channels = Val}) -> Val;
info(exchange, #proc_state{exchange = Val}) -> Val;
info(adapter_info, #proc_state{adapter_info = Val}) -> Val;
info(ssl_login_name, #proc_state{ssl_login_name = Val}) -> Val;
info(retainer_pid, #proc_state{retainer_pid = Val}) -> Val;
info(user, #proc_state{auth_state = #auth_state{username = Val}}) -> Val;
info(vhost, #proc_state{auth_state = #auth_state{vhost = Val}}) -> Val;
info(host, #proc_state{adapter_info = #amqp_adapter_info{host = Val}}) -> Val;
info(port, #proc_state{adapter_info = #amqp_adapter_info{port = Val}}) -> Val;
info(peer_host, #proc_state{adapter_info = #amqp_adapter_info{peer_host = Val}}) -> Val;
info(peer_port, #proc_state{adapter_info = #amqp_adapter_info{peer_port = Val}}) -> Val;
info(protocol, #proc_state{adapter_info = #amqp_adapter_info{protocol = Val}}) ->
    case Val of
        {Proto, Version} -> {Proto, rabbit_data_coercion:to_binary(Version)};
        Other -> Other
    end;
info(channels, PState) -> additional_info(channels, PState);
info(channel_max, PState) -> additional_info(channel_max, PState);
info(frame_max, PState) -> additional_info(frame_max, PState);
info(client_properties, PState) -> additional_info(client_properties, PState);
info(ssl, PState) -> additional_info(ssl, PState);
info(ssl_protocol, PState) -> additional_info(ssl_protocol, PState);
info(ssl_key_exchange, PState) -> additional_info(ssl_key_exchange, PState);
info(ssl_cipher, PState) -> additional_info(ssl_cipher, PState);
info(ssl_hash, PState) -> additional_info(ssl_hash, PState);
info(Other, _) -> throw({bad_argument, Other}).


additional_info(Key,
                #proc_state{adapter_info =
                                #amqp_adapter_info{additional_info = AddInfo}}) ->
    proplists:get_value(Key, AddInfo).
