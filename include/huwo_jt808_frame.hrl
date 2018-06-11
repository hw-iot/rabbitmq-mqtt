-define(PROTOCOL_NAMES,  [{<<"201.1.1">>, "JT808-201.1"}, {<<"201.1.1-huwo">>, "JT808-201.1-huwo"}]).

-define(FB_7E, 16#7E).
-define(FB_3E, 16#3E).

-define(FLAG_BOUNDARY, ?FB_7E).
-define(FLAG_BOUNDARYS, [?FB_7E, ?FB_3E]).
-define(IS_FLAG_BOUNDARY(Val), lists:member(Val, ?FLAG_BOUNDARYS)).

%%----------------------------------------- enum
-define(RESERVED, 0).

-define(NO_SEGMENT,   0).
-define(NEED_SEGMENT, 1).

-define(NO_ENCRYPT,  0).
-define(RSA_ENCRYPT, 1).

-define(STRING0(Val), (list_to_binary(Val))/binary, <<0,0>>/binary).

%%----------------------------------------- data types
-define(UINT8_OF(Val),   Val:8).
-define(UINT16_OF(Val),  Val:16).
-define(UINT32_OF(Val),  Val:32).
-define(UINT_OF(Val, N), Val:(N*8)).
-define(BYTE_OF(Val),    Val:1/binary).
-define(WORD_OF(Val),    Val:2/binary).
-define(DWORD_OF(Val),   Val:4/binary).
-define(BYTES_OF(Val, N),Val:N/binary).
-define(BCD_OF(Val, N),  (bin_utils:bcd_encode(Val, N))/binary).

%%-----------------------------------------
-define(UINT8(Val),  Val:8).
-define(UINT16(Val), Val:16).
-define(UINT32(Val), Val:32).
-define(BYTE(Val),   Val:1/binary).
-define(WORD(Val),   Val:2/binary).
-define(BCD(Val, N), Val:N/binary).

-define(BCD_VALUE(Val), bin_utils:bcd_decode(Val)).
%% TODO return string?
%% returned binary
-define(PARSE_STRING0(Payload, Key, Rest), [Key, Rest] = binary:split(Payload, [<<0,0>>])).
%% returned int
-define(PARSE_UINT8(Payload, Key, Rest), << Key:8, Rest/binary >> = Payload).

%%-----------------------------------------
-define(CLIENTACK,  16#0001).
-define(SERVERACK,  16#8001).
-define(HEARTBEAT,  16#0002).

-define(SIGNIN,     16#0102).
-define(SIGNUP,     16#0100).
-define(SIGNUPACK,  16#8100).
-define(SIGNOUT,    16#0003).

-define(GPSREPORT,  16#0200).

%% -define(CONNECT,    16#0103).
%% -define(DISCONNECT, 16#0108).

%% -define(GATEWAY,    16#0102).

%% -define(GPSV1,      16#0203).
%% -define(GPSV2,      16#0205).

%% -define(NEARBYCLIENT,  16#A201).
%% -define(REQEST_OK ,    16#B201).
%% -define(REQUEST_ERROR, 16#8001).

%% -define(PUSHACK ,  16#0001). % = CLIENTACK
%% -define(PUSH ,  16#8500).

%% connect return codes
-define(CONNACK_ACCEPT,      0).
-define(CONNACK_PROTO_VER,   1). %% unacceptable protocol version
-define(CONNACK_INVALID_ID,  2). %% identifier rejected
-define(CONNACK_SERVER,      3). %% server unavailable
-define(CONNACK_CREDENTIALS, 4). %% bad user name or password
-define(CONNACK_AUTH,        5). %% not authorized

-define(QOS_0, 0).
-define(QOS_1, 1).
-define(QOS_2, 2).

-type message_id() :: any().

-record(parse_state, {boundary,
                      segment_num,
                      segment_sn}).

-record(huwo_jt808_frame, {header, payload}).

-record(huwo_jt808_frame_header,
        {
         client_id, % mobile
         message_id,
         length,
         message_sn
         %% id,
         %% aes     = 0,
         %% zip     = 0,
         %% divide  = 0,
         %% timestamp,
         %% sn
        }).

-record(huwo_jt808_frame_signin,
        {
         clean_sess  = true,
         keep_alive  = 60,
         will_retain = false,
         will_qos    = 0,
         will_flag   = false,
         will_msg,
         will_topic,

         token % string
        }).

%% -record(huwo_jt808_frame_connect,
%%         {
%%          client_id, % = client_type + mobile
%%          clean_sess  = true,
%%          keep_alive  = 60,
%%          will_retain = false,
%%          will_qos    = 0,
%%          will_flag   = false,
%%          will_msg,
%%          will_topic,

%%          mobile,
%%          client_name,
%%          username,  % string | list
%%          password,  % string | list
%%          client_type,
%%          phone_model,
%%          proto_ver,
%%          phone_os,
%%          work_mode = 0
%%         }).

-record(huwo_jt808_frame_ack,
        {
         ack_sn,
         ack_id,
         ack_code
        }).

-record(huwo_jt808_frame_unknown,
        {
         foo,
         bar
        }).

%% TODO: 需要研究消息内容对JT808协议是否有用途，如何修改
-record(huwo_jt808_msg,       {retain :: boolean(),
                               qos :: ?QOS_0 | ?QOS_1 | ?QOS_2,
                               topic :: string(),
                               dup :: boolean(),
                               message_id :: message_id(),
                               payload :: binary()}).

-type huwo_jt808_msg() :: #huwo_jt808_msg{}.


%% --------------------------------------------------------
%% common struct
-record(huwo_topic,          {name,
                              qos}).

%%  为了开发时兼容原mqtt协议的部分代码
%%  TODO: 完全替换为jt808需要去掉此部分

-record(mqtt_frame, {fixed,
                     variable,
                     payload}).

-record(mqtt_frame_fixed,    {type   = 0,
                              dup    = 0,
                              qos    = 0,
                              retain = 0}).
-record(mqtt_frame_publish,  {topic_name,
                              message_id}).

-record(mqtt_topic,          {name,
                              qos}).

-record(mqtt_msg,            {retain :: boolean(),
                              qos :: ?QOS_0 | ?QOS_1 | ?QOS_2,
                              topic :: string(),
                              dup :: boolean(),
                              message_id :: message_id(),
                              payload :: binary()}).
