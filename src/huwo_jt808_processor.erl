-module(huwo_jt808_processor).

-export([
         process_frame/2,
         send_client/2
        ]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("rabbit_mqtt_frame.hrl").
-include("rabbit_mqtt.hrl").
-include("include/huwo_jt808_frame.hrl").


%% 如果不是注册设备的消息，但状态中的connection没定义说明没有注册就发送其他信息
process_frame(#huwo_jt808_frame{ header = #huwo_jt808_frame_header{ id = MsgId}},
              PState = #proc_state{ connection = undefined }) %%
            when MsgId =/= ?MSG_ID_REG ->
                {error, connect_expected, PState};
%% 开始处理包
process_frame(Frame = #huwo_jt808_frame{ header = #huwo_jt808_frame_header{ id = MsgId }},
              PState) ->
    case process_request(MsgId, Frame, PState) of
        {ok, PState1} -> {ok, PState1, PState1#proc_state.connection};
        Ret -> Ret
    end.

process_request(?MSG_ID_REG,
                Frame,
                PState0 = #proc_state{ ssl_login_name = SSLLoginName,
                                       send_fun       = SendFun,
                                       adapter_info   = AdapterInfo = #amqp_adapter_info{additional_info = Extra} }) ->
    % TODO: 验证登录
    % TODO: 给当前用户开推送队列，用于服务端推送
    bin_utils:dump(process_request, Frame),
    {ok, PState1};

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
