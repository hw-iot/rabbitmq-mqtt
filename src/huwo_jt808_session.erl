%% TODO segment

-module(huwo_jt808_session).

-author("Luo Tao <lotreal@gmail.com>").

-export([response/2]).

-include("huwo_jt808_frame.hrl").

%% -spec reply(#huwo_jt808_frame(), integer()) -> {ok | error, #huwo_jt808()}.
response(#huwo_jt808_frame{header = #huwo_jt808_frame_header{
                                       client_id = Mobile, message_id = ID, message_sn = SN}},
         ReturnCode) ->
    #huwo_jt808_frame{
       header = #huwo_jt808_frame_header{
                   client_id  = Mobile,
                   message_id = ?SERVERACK,
                   message_sn = SN + 1},
       payload = #huwo_jt808_frame_ack{
                    ack_sn = SN,
                    ack_id = ID,
                    ack_code = ReturnCode}}.
