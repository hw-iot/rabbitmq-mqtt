%% TODO segment

-module(huwo_jt808_frame).

-author("Luo Tao <lotreal@gmail.com>").

-export([initial_state/0]).
-export([parse/2, parse/1, serialise/1]).
-export([parse_content/2]).
-export([escape/1]).
-export([dump/1]).

-include("huwo_jt808_frame.hrl").


initial_state() -> none.

%% API
parse(Frame, _) ->
    parse(Frame).

parse(Frame) ->
    bin_utils:dump(frame_parse_frame, Frame),
    Content = parse_content(Frame, ?FLAG_BOUNDARY),
    %% bin_utils:dump(frame_parse_content, Content),
    <<Id:16, Property:2/binary, Timestamp:6/binary, SN:16, Rest/binary>> = Content,
    <<Aes:1, Zip:1, Divide:1, Len:13>> = Property,
    Header = #huwo_jt808_frame_header{
                id        = Id,
                aes       = Aes,
                zip       = Zip,
                divide    = Divide,
                length    = Len,
                timestamp = bin_utils:bcd_decode(Timestamp),
                sn        = SN},
    <<Body:Len/binary, _:8>> = Rest,
    {ok, Payload} = parse_body(Header, Body),
    Request = #huwo_jt808_frame{header = Header, payload = Payload},
    bin_utils:dump(frame_parse_result, Request),
    {ok, Request}.

parse_body(#huwo_jt808_frame_header{ id = ?CONNECT }, Body) ->
    ?PARSE_STRING0(Body,  Mobile,      Rest1),
    ?PARSE_STRING0(Rest1,    ClientName,  Rest2),
    ?PARSE_STRING0(Rest2,    Username,    Rest3),
    ?PARSE_STRING0(Rest3,    Password,    Rest4),
    ?PARSE_UINT8  (Rest4,    ClientType,  Rest5),
    ?PARSE_STRING0(Rest5,    PhoneModel,  Rest6),
    ?PARSE_STRING0(Rest6,    ProtoVer,    Rest7),
    ?PARSE_UINT8  (Rest7,    WorkMode,    _Rest8),
    {ok, #huwo_jt808_frame_connect{
            mobile = Mobile,
            client_name = ClientName,
            username = Username,
            password = Password,
            client_type = ClientType,
            phone_model = PhoneModel,
            proto_ver = ProtoVer,
            phone_os = ProtoVer,
            work_mode = WorkMode}};
parse_body(_Header, Body) ->
    {ok, Body}.

serialise(#huwo_jt808_frame{
             payload = #huwo_jt808_frame_connect{
                          mobile = Mobile,
                          client_name = ClientName,
                          username = Username,
                          password = Password,
                          client_type = ClientType,
                          phone_model = PhoneModel,
                          proto_ver = ProtoVer,
                          phone_os = PhoneOS,
                          work_mode = WorkMode
                         }} = Request) ->
    Payload = <<?STRING0(Mobile),
                ?STRING0(ClientName), ?STRING0(Username), ?STRING0(Password), ClientType:8,
                ?STRING0(PhoneModel), ?STRING0([ ProtoVer | PhoneOS ]),
                WorkMode:8>>,
    serialise(?FRAME(?CONNECT, Request, Payload));

serialise(#huwo_jt808_frame{
             header = #huwo_jt808_frame_header{
                         id = Id,
                         aes = Aes,
                         zip = Zip,
                         divide = Divide,
                         timestamp = Timestamp,
                         sn = SN
                        },
             payload = Payload
            }) ->
    BCDTimestamp = bin_utils:bcd_encode(Timestamp, 6),
    Len = byte_size(Payload),
    Checksum = checksum(Payload),
    Property = <<Aes:1, Zip:1, Divide:1, Len:13>>,
    Body = <<Id:16,
             Property:2/binary,
             BCDTimestamp:6/binary,
             SN:16,
             Payload:Len/binary,
             Checksum:1/binary>>,
    <<?FLAG_BOUNDARY, (escape(Body))/binary, ?FLAG_BOUNDARY>>.


%% internal
parse_content(Frame, Flag) ->
    parse_content(Frame, Flag, <<>>).

parse_content(<<Flag>>, Flag, Acc) ->
    unescape(Acc);
parse_content(<<Flag, Rest/binary>>, Flag, Acc) ->
    parse_content(Rest, Flag, Acc);
parse_content(<<H, Rest/binary>>, Flag, Acc) ->
    parse_content(Rest, Flag, <<Acc/binary, H>>).


escape(Body) -> escape(Body, <<>>).
escape(<<>>, Acc) -> Acc;
escape(<<16#3D, T/binary>>, Acc) ->
    escape(T, <<Acc/binary,(?ESCAPED_3D)/binary>>);
escape(<<16#3E, T/binary>>, Acc) ->
    escape(T, <<Acc/binary,(?ESCAPED_3E)/binary>>);
escape(<<H:1/binary, T/binary>>, Acc) ->
    escape(T, <<Acc/binary, H/binary>>).

unescape(Body) -> unescape(Body, <<>>).
unescape(<<>>, Acc) -> Acc;
unescape(<<16#3D, 16#01, T/binary>>, Acc) ->
    unescape(T, <<Acc/binary, 16#3D>>);
unescape(<<16#3D, 16#02, T/binary>>, Acc) ->
    unescape(T, <<Acc/binary, 16#3E>>);
unescape(<<H:1/binary, T/binary>>, Acc) ->
    unescape(T, <<Acc/binary, H/binary>>).

%% calculate jt808 checksum
checksum(Data) ->                 checksum(iolist_to_binary(Data), 0).
checksum(<<I, T/binary>>, Acc) -> checksum(T,  Acc bxor I);
checksum(<<>>, Acc) ->            <<Acc>>.

%% debug
dump(#huwo_jt808_frame{
        header = #huwo_jt808_frame_header{
                    id        = Id,
                    aes       = Aes,
                    zip       = Zip,
                    divide    = Divide,
                    length    = Len,
                    timestamp = Timestamp,
                    sn        = SN},
        payload = Payload}) ->
    bin_utils:dump(property, [Id, Aes, Zip, Divide, Len]),
    bin_utils:dump(timestamp, Timestamp),
    bin_utils:dump(sn, SN),
    bin_utils:dump(payload, Payload).
