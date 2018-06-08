%% TODO segment

-module(huwo_jt808_frame).

-author("Luo Tao <lotreal@gmail.com>").

-export([initial_state/0]).
-export([parse/2, serialise/1]).
-export([parse_content/2]).
-export([escape/1]).
-export([dump/1]).

-include("huwo_jt808.hrl").
-include("huwo_jt808_frame.hrl").

-define(MAX_LEN, 2#1111111111111).

initial_state() -> none.

%% API
parse(<<>>, none) ->
    {more, fun(Bin) -> parse(Bin, none) end};
parse(<<?FLAG_BOUNDARY, Id:16, Property:2/binary, Timestamp:6/binary, SN:16, Rest/binary>>, none) ->
    <<Aes:1, Zip:1, Divide:1, Len:13>> = Property,
    Header = #huwo_jt808_frame_header{
                id        = Id,
                aes       = Aes,
                zip       = Zip,
                divide    = Divide,
                length    = Len,
                timestamp = bin_utils:bcd_decode(Timestamp),
                sn        = SN},
    parse_frame(Rest, Header, Len);
parse(Bin, Cont) ->
    bin_utils:dump(frame_parse_func, Cont),
    Cont(Bin).

%% Bin: received data
%% Header: jt808 header
%% Len: int, defined in header
%% ->
%% {error, Reason}
%% {more, ParseFunc}
%% {ok, #huwo_jt808_frame, Rest}
parse_frame(_Bin, _Header, Len) when Len > ?MAX_LEN ->
    {error, invalid_jt808_frame_len};
parse_frame(<<>>, Header, Len) ->
    {more, fun(Bin) -> parse_frame(Bin, Header, Len) end};
parse_frame(Bin, Header, Len) ->
    case Bin of
        <<Body:Len/binary, _CheckSum:8, ?FLAG_BOUNDARY, Rest/binary>> ->
            case parse_body(Header, Body) of
                {ok, Payload} ->
                    {ok, #huwo_jt808_frame{header = Header, payload = Payload}, Rest};
                _ ->
                    {error, frame_data_corrupt}
            end;
        TooShortBin ->
            {more, fun(BinMore) ->
                           parse_frame(<<TooShortBin/binary, BinMore/binary>>, Header, Len)
                   end}
    end.

parse_body(#huwo_jt808_frame_header{ id = ?CONNECT }, Body) ->
    ?PARSE_STRING0(Body,  Mobile,      Rest1),
    ?PARSE_STRING0(Rest1,    ClientName,  Rest2),
    ?PARSE_STRING0(Rest2,    Username,    Rest3),
    ?PARSE_STRING0(Rest3,    Password,    Rest4),
    ?PARSE_UINT8  (Rest4,    ClientType,  Rest5),
    ?PARSE_STRING0(Rest5,    PhoneModel,  Rest6),
    ?PARSE_STRING0(Rest6,    ProtoVer,    Rest7),
    ?PARSE_STRING0(Rest7,    ProtoOS,     Rest8),
    ?PARSE_UINT8  (Rest8,    WorkMode,    _),
    ClientId = binary_to_list(<< (integer_to_binary(ClientType))/binary, Mobile/binary >>),
    {ok, #huwo_jt808_frame_connect{
            client_id = ClientId,
            mobile = Mobile,
            client_name = ClientName,
            username = binary_to_list(Username),
            password = binary_to_list(Password),
            client_type = ClientType,
            phone_model = PhoneModel,
            proto_ver = ProtoVer,
            phone_os = ProtoOS,
            work_mode = WorkMode}};
parse_body(_AnyHeader, Body) ->
    {ok, Body}.

%% serialise(huwo_jt808_frame())
serialise(#huwo_jt808_frame{ header = Header, payload = Payload0 }) ->
    Payload = serialise_payload(Payload0),
    serialise_frame(Header, Payload);
serialise(Bin) ->
    Bin.

serialise_payload(#huwo_jt808_frame_connect{
                     mobile = Mobile,
                     client_name = ClientName,
                     username = Username,
                     password = Password,
                     client_type = ClientType,
                     phone_model = PhoneModel,
                     proto_ver = ProtoVer,
                     phone_os = PhoneOS,
                     work_mode = WorkMode
                    }) ->
    <<?STRING0(Mobile),
      ?STRING0(ClientName), ?STRING0(Username), ?STRING0(Password), ClientType:8,
      ?STRING0(PhoneModel), ?STRING0(ProtoVer), ?STRING0(PhoneOS),
      WorkMode:8>>;
serialise_payload(#huwo_jt808_frame_ack{
                     ack_sn = SN,
                     ack_id = ID,
                     ack_code = ReturnCode}) ->
    <<?UINT16(SN), ?UINT16(ID), ?UINT8(ReturnCode)>>;
serialise_payload(Any) ->
    term_to_binary(Any).

serialise_frame(#huwo_jt808_frame_header{
                   id = Id,
                   aes = Aes,
                   zip = Zip,
                   divide = Divide,
                   timestamp = Timestamp,
                   sn = SN
                  }, Payload)->
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
