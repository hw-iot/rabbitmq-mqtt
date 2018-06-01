-module(huwo_jt808_processor).

-export([process_frame/2]).


%% 如果不是注册设备的消息，但状态中的connection没定义说明没有注册就发送其他信息
process_frame(#huwo_jt808_frame{ header = #huwo_jt808_frame_header{ id = MsgId}},
              PState = #proc_state{ connection = undefined }) %%
            when MsgId =/= ?MSG_ID_REG ->
                {error, connect_expected, PState};
%% 开始处理包
rocess_frame(Frame = #huwo_jt808_frame{ header = #huwo_jt808_frame_header{ id = MsgId }},
              PState) ->
    case process_request(MsgId, Frame, PState) of
        {ok, PState1} -> {ok, PState1, PState1#proc_state.connection};
        Ret -> Ret
    end.
%process_frame(Frame, PState) ->
%    Type = 1,
%    process_request(Type, Frame, PState),
%    ok.

process_request(?MSG_ID_REG,
                Frame,
                PState0 = #proc_state{ ssl_login_name = SSLLoginName,
                                       send_fun       = SendFun,
                                       adapter_info   = AdapterInfo = #amqp_adapter_info{additional_info = Extra} }) ->
    % TODO: 验证登录
    % TODO: 给当前用户开推送队列，用于服务端推送
    bin_utils:dump(process_request, Frame),
    {ok, PState1};
process_request(_Type, Frame, _PState) ->
    bin_utils:dump(process_request, Frame).
