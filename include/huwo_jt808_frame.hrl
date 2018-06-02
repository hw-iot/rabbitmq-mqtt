-define(FLAG_BOUNDARY, 16#3E).
-define(ESCAPED_3E, <<16#3D, 16#02>>).
-define(ESCAPED_3D, <<16#3D, 16#01>>).

-define(MSG_ID_REG,16#0103).

-define(SEP, <<0,0>>).
-define(STRING0(Val), (list_to_binary(Val))/binary, ?SEP/binary).


-define(CONNECT,    16#0103).
-define(DISCONNECT, 16#0108).
-define(CLIENTACK,  16#0001).
-define(SERVERACK,  16#8001).
-define(GATEWAY,    16#0102).
-define(HEARTBEAT,  16#0105).
-define(GPSV1,      16#0203).
-define(GPSV2,      16#0205).

-define(QOS_0, 0).
-define(QOS_1, 1).
-define(QOS_2, 2).

-type message_id() :: any().

%% -record(huwo_jt808_frame_options,
%%         {boundary = ?FLAG_BOUNDARY}).

-record(huwo_jt808_frame, {header, payload}).

-record(huwo_jt808_frame_header,
        { id,
          aes     = 0,
          zip     = 0,
          divide  = 0,
          length,
          timestamp,
          sn }).

-record(huwo_jt808_frame_connect,
        { mobile,
	  client_name,
          username,
          password,
          client_type,
          phone_model,
          proto_ver,
          phone_os,
          work_mode = 0 }).

%% TODO: 需要研究消息内容对JT808协议是否有用途，如何修改
-record(huwo_jt808_msg,       {retain :: boolean(),
                               qos :: ?QOS_0 | ?QOS_1 | ?QOS_2,
                               topic :: string(),
                               dup :: boolean(),
                               message_id :: message_id(),
                               payload :: binary()}).

-type huwo_jt808_msg() :: #huwo_jt808_msg{}.
