%% TODO segment

-module(huwo_jt808_session).

-author("Luo Tao <lotreal@gmail.com>").

-export([warp/1]).

-export([response/2]).

-include("huwo_jt808_frame.hrl").

warp({ID, SN, Var})->
    #huwo_jt808_frame{
       header = header(ID, SN),
       payload = Var}.

header(ID, SN)->
    #huwo_jt808_frame_header{
       message_id = ID,
       message_sn = SN}.

%% -spec reply(#huwo_jt808_frame(), integer()) -> {ok | error, #huwo_jt808()}.
response(#huwo_jt808_frame{header = #huwo_jt808_frame_header{ message_id = ID, message_sn = SN}}, ReturnCode) ->
    warp({?SERVERACK, SN + 1,
          #huwo_jt808_frame_ack{
             ack_sn = SN,
             ack_id = ID,
             ack_code = ReturnCode}}).
