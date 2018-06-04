-module(huwo_jt808_processor).


-export([info/2, initial_state/2, initial_state/4]).
-export([amqp_callback/2, send_will/1]).
-export([
         process_frame/2,
         send_client/2,
         close_connection/1
        ]).

-include_lib("amqp_client/include/amqp_client.hrl").
%%-include("rabbit_mqtt_frame.hrl").
-include("rabbit_mqtt.hrl").
-include("include/huwo_jt808_frame.hrl").

initial_state(Socket, SSLLoginName) ->
    RealSocket = rabbit_net:unwrap_socket(Socket),
    initial_state(RealSocket, SSLLoginName,
                  adapter_info(Socket, 'MQTT'),
                  fun send_client/2).

initial_state(Socket, SSLLoginName,
              AdapterInfo0 = #amqp_adapter_info{additional_info = Extra},
              SendFun) ->
    %% JT808 connections use exactly one channel. The frame max is not
    %% applicable and there is no way to know what client is used.
    AdapterInfo = AdapterInfo0#amqp_adapter_info{additional_info = [
                                                                    {channels, 1},
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


%%-----------------------------------------------------------------------

%% amqp_pub()

%% 自定义的amqp_pub
%% TODO: 用系统的amqp函数替换这个自定义的
amqp_pub(#huwo_jt808_frame{
            payload = Payload
           }) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"metronome">>,
                                                   type = <<"topic">>}),

    RoutingKey = <<"anonymous.info">>,
    amqp_channel:cast(Channel,
                      #'basic.publish'{
                         exchange = <<"metronome">>,
                         routing_key = RoutingKey},
                      #amqp_msg{payload = Payload}),

    io:format(" [x] Sent ~p:~p~n", [RoutingKey, Payload]),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.


%%-----------------------------------------
-define(MAGIC, 42).

%% 开始处理包
%% debug hello package
process_frame(Frame = #huwo_jt808_frame{ header = #huwo_jt808_frame_header{ id = ?MAGIC}},
              PState) ->
    case process_request(?MAGIC, Frame, PState) of
        {ok, PState1} -> {ok, PState1, PState1#proc_state.connection};
        Ret -> Ret
    end;
%% 消息头已解析，可以取得消息类型MsgID。消息体 Payload 为二进制，待进一步解析
process_frame(#huwo_jt808_frame{
                 header = #huwo_jt808_frame_header{
                             id = MsgID}} = Frame, PState) ->
    bin_utils:dump(process_frame_frame, Frame),
    case process_request(MsgID, Frame, PState) of
        {ok, PState1} -> {ok, PState1, PState1#proc_state.connection};
        Ret -> Ret
    end.

%% 如果不是注册设备的消息，但状态中的connection没定义说明没有注册就发送其他信息
%% process_frame(#huwo_jt808_frame{ header = #huwo_jt808_frame_header{ id = MsgId}},
%%               PState = #proc_state{ connection = undefined }) %%
%%             when MsgId =/= ?MSG_ID_REG ->
%%                 {error, connect_expected, PState}.


%% process_request()


process_request(?CONNECT,
                #huwo_jt808_frame{
                   payload = #huwo_jt808_frame_connect{
                                mobile = _Mobile,
                                client_name = _ClientName,
                                username = _Username,
                                password = _Password,
                                client_type = _ClientType,
                                phone_model = _PhoneModel,
                                proto_ver = _ProtoVer,
                                phone_os = _ProtoVer,
                                work_mode = _WorkMode} = Payload},
                PState0) ->
    bin_utils:dump(process_request_connect_payload, Payload),
    {ok, PState0};

%% 设备注册
%% process_request(?MSG_ID_REG,
%%                 Frame,
%%                 PState0 = #proc_state{ ssl_login_name = _SSLLoginName,
%%                                        send_fun       = _SendFun,
%%                                        adapter_info   = _AdapterInfo = #amqp_adapter_info{additional_info = _Extra} }) ->
%% %% TODO: 验证登录
%% %% TODO: 给当前用户开推送队列，用于服务端推送
%%     bin_utils:dump(process_request, Frame),
%%     {ok, PState0};




%% 目前用于测试
process_request(_MessageType,
                _Frame,
                PState0 = #proc_state{ send_fun       = _SendFun }) ->
    Frame = #huwo_jt808_frame{
               header = #huwo_jt808_frame_header{
                           id = 42,
                           timestamp = 201806011200,
                           sn = 1},
               payload = <<"hello, 808!", 16#3D, 16#3E>>},

    bin_utils:dump(process_request, Frame),
    %% SendFun = send_client/2,
    send_client(Frame, PState0),
    amqp_pub(Frame),
    {ok, PState0}.

send_client(Frame, #proc_state{ socket = Sock }) ->
    bin_utils:dump(send_client_frame, Frame),

    Package = huwo_jt808_frame:serialise(Frame),
    bin_utils:dump(send_client_package, Package),
    rabbit_net:port_command(Sock, Package).





%%---------------------------------------------------------------------
%% sys

hand_off_to_retainer(RetainerPid, Topic, #huwo_jt808_msg{payload = <<"">>}) ->
    %% TODO: retainer支持
    rabbit_mqtt_retainer:clear(RetainerPid, Topic),
    ok;
hand_off_to_retainer(RetainerPid, Topic, Msg) ->
    %% TODO: retainer支持
    rabbit_mqtt_retainer:retain(RetainerPid, Topic, Msg),
    ok.


%% send_will()

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


delivery_mode(?QOS_0) -> 1;
delivery_mode(?QOS_1) -> 2.

%% amqp_pub

amqp_pub(undefined, PState) ->
    PState;

%% set up a qos1 publishing channel if necessary
%% this channel will only be used for publishing, not consuming
amqp_pub(Msg   = #huwo_jt808_msg{ qos = ?QOS_1 },
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
                                   rabbit_mqtt_util:mqtt2amqp(Topic)}, %% TODO: 协议转换需要替换
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
    amqp_chnnel:cast_flow(Ch, Method, Msg),
    PState #proc_state{ unacked_pubs   = UnackedPubs1,
                        awaiting_seqno = SeqNo1 }.


%% amqp_callback
%% TODO: 从mqtt frame 提取的，mqtt的消息类型，换为JT808后要去掉
-define(PUBLISH,      3).
-define(PUBACK,       4).
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
            SendFun(
              %% TODO: 替换为JT808报文
              #mqtt_frame{ fixed = #mqtt_frame_fixed{
                                      type = ?PUBLISH,
                                      qos  = DeliveryQos,
                                      dup  = Dup },
                           variable = #mqtt_frame_publish{
                                         message_id =
                                             case DeliveryQos of
                                                 ?QOS_0 -> undefined;
                                                 ?QOS_1 -> MsgId
                                             end,
                                         topic_name =
                                             %% TODO: 替换为JT808
                                             rabbit_mqtt_util:amqp2mqtt(
                                               RoutingKey) },
                           payload = Payload}, PState),
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

%% decide at which qos level to deliver based on subscription
%% and the message publish qos level. non-MQTT publishes are
%% assumed to be qos 1, regardless of delivery_mode.
delivery_qos(Tag, _Headers,  #proc_state{ consumer_tags = {Tag, _} }) ->
    {?QOS_0, ?QOS_0};
delivery_qos(Tag, Headers,   #proc_state{ consumer_tags = {_, Tag} }) ->
                                                % TODO: 替换为JT808
    case rabbit_mqtt_util:table_lookup(Headers, <<"x-mqtt-publish-qos">>) of
        {byte, Qos} -> {lists:min([Qos, ?QOS_1]), ?QOS_1};
        undefined   -> {?QOS_1, ?QOS_1}
    end.

%% TODO: messageid需要处理
ensure_valid_mqtt_message_id(Id) when Id >= 16#ffff ->
    1;
ensure_valid_mqtt_message_id(Id) ->
    Id.

%%safe_max_id(Id0, Id1) ->
%%    ensure_valid_mqtt_message_id(erlang:max(Id0, Id1)).

next_msg_id(PState = #proc_state{ message_id = MsgId0 }) ->
    MsgId1 = ensure_valid_mqtt_message_id(MsgId0 + 1),
    PState#proc_state{ message_id = MsgId1 }.

adapter_info(Sock, ProtoName) ->
    amqp_connection:socket_adapter_info(Sock, {ProtoName, "N/A"}).

close_connection(PState = #proc_state{ connection = undefined }) ->
    PState;
close_connection(PState = #proc_state{ connection = Connection,
                                       client_id  = ClientId }) ->
    %% todo: maybe clean session
    case ClientId of
        undefined -> ok;
        %% TODO: ??? 需要解除MQTT依赖
        _         -> ok = huwo_jt808_collector:unregister(ClientId, self())
    end,
    %% ignore noproc or other exceptions to avoid debris
    catch amqp_connection:close(Connection),
    PState #proc_state{ channels   = {undefined, undefined},
                        connection = undefined }.



check_topic_access(TopicName, Access,
                   #proc_state{
                      auth_state = #auth_state{user = User = #user{username = Username},
                                               vhost = VHost},
                      exchange = Exchange,
                      client_id = ClientId}) ->
    Resource = #resource{virtual_host = VHost,
                         kind = topic,
                         name = Exchange},
                                                % TODO: mqtt转amqp
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

%% info()
%% 返回进程状态的指定值

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

%%---------------------------------------------------------------------
%% private
