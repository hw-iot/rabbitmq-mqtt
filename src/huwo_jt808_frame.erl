%% TODO segment

-module(huwo_jt808_frame).

-author("Luo Tao <lotreal@gmail.com>").

-export([initial_state/0]).
-export([parse/2, serialise/1]).
-export([escape/2, unescape/2]).
-export([dump/1]).

-include("huwo_jt808.hrl").
-include("huwo_jt808_frame.hrl").

-define(MAX_LEN, 2#1111111111111).

initial_state() -> none.

%% API
parse(<<>>, none) ->
    {more, fun(Bin) -> parse(Bin, none) end};
parse(<<FlagBoundary:8, Rest/binary>>, none) ->
    case lists:member(FlagBoundary, ?FLAG_BOUNDARYS) of
        true ->
            parse_header(Rest, #parse_state{flag_boundary = FlagBoundary});
        _ ->
            parse(Rest, none)
    end;
parse(Bin, Cont) ->
    bin_utils:dump(frame_parse_func, Cont),
    Cont(Bin).

parse_header(<<>>, ParseState) ->
    {more, fun(Bin) -> parse_header(Bin, ParseState) end};
parse_header(<<?UINT16(MessageID),
               ?WORD(MessageProperty),
               ?BCD(Mobile, 6),
               ?UINT16(MessageSN),
               Rest0/binary>>,
             ParseState0) ->
    <<_Reserved:2, Segmentation:1, _Encryption:3, Length:10>>
        = MessageProperty,
    {Rest, ParseState} =
        case Segmentation of
            ?NO_SEGMENT -> {Rest0, ParseState0};
            _ -> <<?UINT16(SegmentNum), ?UINT16(SegmentSN), Rest1/binary>>
                     = Rest0,
                 {Rest1, ParseState0#parse_state{
                           segment_num = SegmentNum,
                           segment_sn = SegmentSN
                          }}
        end,
    Header = #huwo_jt808_frame_header{
                message_id = MessageID,
                message_sn = MessageSN,
                mobile = ?BCD_VALUE(Mobile),
                length = Length
               },
    parse_frame(Rest, Header, Length, ParseState).

%% Bin: received data
%% Header: jt808 header
%% Len: int, defined in header
%% ->
%% {error, Reason}
%% {more, ParseFunc}
%% {ok, #huwo_jt808_frame, Rest}
parse_frame(_Bin, _Header, Len, _ParseState) when Len > ?MAX_LEN ->
    {error, invalid_jt808_frame_len};
parse_frame(<<>>, Header, Len, ParseState) ->
    {more, fun(Bin) -> parse_frame(Bin, Header, Len, ParseState) end};
parse_frame(TooShortBin, Header, Len, ParseState)
  when byte_size(TooShortBin) < Len ->
    {more, fun(BinMore) ->
                   parse_frame(<<TooShortBin/binary, BinMore/binary>>,
                               Header, Len, ParseState)
           end};
parse_frame(Bin, Header, Len,
            #parse_state{flag_boundary = FlagBoundary} = ParseState) ->
    case Bin of
        <<Body0:Len/binary, ?UINT8(CheckSum0),
          ?UINT8(FlagBoundary), Rest/binary>> ->
            CheckSum = checksum(Body0),
            ?DEBUG(checksum, CheckSum),
            ?DEBUG(checksum, CheckSum0),
            ?DEBUG(checksum, {CheckSum0, CheckSum}),
            case CheckSum == CheckSum0 of
                true ->
                    Body = unescape(Body0, FlagBoundary),
                    case parse_body(Header, Body) of
                        {ok, Payload} ->
                            {ok, #huwo_jt808_frame{
                                    header = Header, payload = Payload}, Rest};
                        _ ->
                            {error, frame_data_corrupt}
                    end;
                false ->
                    {error, invalid_checksum}
            end;
        _ANY ->
            {more, fun(NewBin) -> parse_frame(NewBin, Header, Len, ParseState) end}
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
                   id = MessageID,
                   sn = MessageSN
                  }, Payload)->
    Mobile = 13896079527,
    Segmentation = ?NO_SEGMENT,
    Encryption = ?NO_ENCRYPT,

    Length = byte_size(Payload),
    CheckSum = checksum(Payload),

    Body = <<?UINT16_OF(MessageID),
             ?WORD_OF(<<Segmentation:3, Encryption:3, Length:10>>),
             ?BCD_OF(Mobile, 6),
             ?UINT16_OF(MessageSN),
             ?BYTES_OF(Payload, Length),
             ?UINT8_OF(CheckSum)>>,
    FlagBoundary = ?FB_7E,
    <<FlagBoundary, (escape(Body, FlagBoundary))/binary, FlagBoundary>>.

%% internal
%% parse_content(Frame, Flag) ->
%%     parse_content(Frame, Flag, <<>>).

%% parse_content(<<Flag>>, Flag, Acc) ->
%%     unescape(Acc);
%% parse_content(<<Flag, Rest/binary>>, Flag, Acc) ->
%%     parse_content(Rest, Flag, Acc);
%% parse_content(<<H, Rest/binary>>, Flag, Acc) ->
%%     parse_content(Rest, Flag, <<Acc/binary, H>>).

escape(Body, ?FB_7E) -> escape_7e(Body, <<>>);
escape(Body, ?FB_3E) -> escape_3e(Body, <<>>).

unescape(Body, ?FB_7E) -> unescape_7e(Body, <<>>);
unescape(Body, ?FB_3E) -> unescape_3e(Body, <<>>).


escape_3e(<<>>, Acc) -> Acc;
escape_3e(<<16#3D, T/binary>>, Acc) ->
    escape_3e(T, <<Acc/binary, 16#3D, 16#01>>);
escape_3e(<<16#3E, T/binary>>, Acc) ->
    escape_3e(T, <<Acc/binary, 16#3D, 16#02>>);
escape_3e(<<H:1/binary, T/binary>>, Acc) ->
    escape_3e(T, <<Acc/binary, H/binary>>).

unescape_3e(<<>>, Acc) -> Acc;
unescape_3e(<<16#3D, 16#01, T/binary>>, Acc) ->
    unescape_3e(T, <<Acc/binary, 16#3D>>);
unescape_3e(<<16#3D, 16#02, T/binary>>, Acc) ->
    unescape_3e(T, <<Acc/binary, 16#3E>>);
unescape_3e(<<H:1/binary, T/binary>>, Acc) ->
    unescape_3e(T, <<Acc/binary, H/binary>>).


escape_7e(<<>>, Acc) -> Acc;
escape_7e(<<16#7D, T/binary>>, Acc) ->
    escape_7e(T, <<Acc/binary, 16#7D, 16#01>>);
escape_7e(<<16#7E, T/binary>>, Acc) ->
    escape_7e(T, <<Acc/binary, 16#7D, 16#02>>);
escape_7e(<<H:1/binary, T/binary>>, Acc) ->
    escape_7e(T, <<Acc/binary, H/binary>>).

unescape_7e(<<>>, Acc) -> Acc;
unescape_7e(<<16#7D, 16#01, T/binary>>, Acc) ->
    unescape_7e(T, <<Acc/binary, 16#7D>>);
unescape_7e(<<16#7D, 16#02, T/binary>>, Acc) ->
    unescape_7e(T, <<Acc/binary, 16#7E>>);
unescape_7e(<<H:1/binary, T/binary>>, Acc) ->
    unescape_7e(T, <<Acc/binary, H/binary>>).

%% calculate jt808 checksum, return integer
checksum(Data) ->                 checksum(iolist_to_binary(Data), 0).
checksum(<<I, T/binary>>, Acc) -> checksum(T,  Acc bxor I);
checksum(<<>>, Acc) ->            Acc.

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
