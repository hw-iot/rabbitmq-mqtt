%% TODO segment

-module(huwo_jt808_frame).

-author("Luo Tao <lotreal@gmail.com>").

-export([initial_state/0]).
-export([parse/2, serialise/1]).

%% debug
-export([escape/2, unescape/2]).
-export([checksum/1]).

-include("huwo_jt808.hrl").
-include("huwo_jt808_frame.hrl").

initial_state() -> none.

%% ...11 21.....2 ... 2..1.....1
parse(<<>>, none) ->
    {more, fun(Bin) -> parse(Bin, none) end};
parse(<<H:8, Rest/binary>>, none) ->
    case ?IS_FLAG_BOUNDARY(H) of
        true  ->
            parse_body(Rest, <<>>, #parse_state{boundary = H});
        false ->
            parse(Rest, none)
    end;
parse(Bin, Cont) ->
    Cont(Bin).

parse_body(<<>>, Body, ParseState) ->
    {more, fun(BinMore) -> parse_body(BinMore, Body, ParseState) end};
parse_body(<<H:8, Rest/binary>>, Body0, #parse_state{boundary = FB} = ParseState) ->
    case H == FB of
        true  ->
            case verify_frame_length(Body0) of
                true  ->
                    Body1 = unescape(Body0, FB),
                    ?DEBUG(checksum, checksum(Body1)),
                    case checksum(Body1) of
                        {true, Body, _CheckSum} ->
                            parse_header(Body, ParseState);
                        {false, _, _} ->
                            {error, invalid_checksum}
                    end;
                false ->
                    {error, invalid_jt808_frame_len}
            end;
        false ->
            parse_body(Rest, <<Body0/binary, H>>, ParseState)
    end.

verify_frame_length(<<>>) ->
    false;
verify_frame_length(_Body) ->
    true.

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
                client_id  = integer_to_list(?BCD_VALUE(Mobile)),
                length     = Length,
                message_sn = MessageSN
               },
    parse_frame(Rest, Header, Length, ParseState).

%% Bin: received data
%% Header: jt808 header
%% Len: int, defined in header
%% ->
%% {error, Reason}
%% {more, ParseFunc}
%% {ok, #huwo_jt808_frame, Rest}
parse_frame(Bin, Header, Len, _ParseState) ->
    case Bin of
        <<Body:Len/binary, _Rest/binary>> ->
            case parse_payload(Header, Body) of
                {ok, Payload} ->
                    {ok, #huwo_jt808_frame{
                            header = Header, payload = Payload}, <<>>};
                _ ->
                    {error, frame_data_corrupt}
            end;
        _ANY ->
            {error, invalid_jt808_frame}
    end.

%%----------------------------------------- [FILL] parse_payload
parse_payload(#huwo_jt808_frame_header{ message_id = ?SIGNIN}, Body) ->
    {ok, #huwo_jt808_frame_signin{token = Body}};
parse_payload(#huwo_jt808_frame_header{ message_id = ?HEARTBEAT}, _Body) ->
    {ok, #huwo_jt808_frame_heartbeat{}};
parse_payload(_AnyHeader, Body) ->
    {ok, Body}.

%% serialise(huwo_jt808_frame())
serialise(#huwo_jt808_frame{ header = Header, payload = Payload0 }) ->
    Payload = serialise_payload(Payload0),
    serialise_frame(Header, Payload);
serialise(Bin) ->
    Bin.

%%----------------------------------------- [FILL] serialise_payload
serialise_payload(#huwo_jt808_frame_signin{token = Token}) -> list_to_binary(Token);
serialise_payload(#huwo_jt808_frame_ack{
                     ack_sn = SN,
                     ack_id = ID,
                     ack_code = ReturnCode}) ->
    <<?UINT16(SN), ?UINT16(ID), ?UINT8(ReturnCode)>>;
serialise_payload(Bin) when is_binary(Bin) -> bin;
serialise_payload(Any) -> term_to_binary(Any).

serialise_frame(#huwo_jt808_frame_header{
                   client_id  = Mobile,
                   message_id = MessageID,
                   message_sn = MessageSN
                  }, Payload)->
    FlagBoundary = ?FB_7E,
    Segmentation = ?NO_SEGMENT,
    Encryption = ?NO_ENCRYPT,

    Length = byte_size(Payload),
    Body = <<?UINT16_OF(MessageID),
             ?WORD_OF(<<Segmentation:3, Encryption:3, Length:10>>),
             ?BCD_OF(Mobile, 6),
             ?UINT16_OF(MessageSN),
             Payload/binary>>,
    %% TODO
    Body1 = <<Body/binary, (calc_checksum(Body))>>,

    <<FlagBoundary, (escape(Body1, FlagBoundary))/binary, FlagBoundary>>.

%% internal
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

%% calculate jt808 checksum, input binary, return boolean
checksum(Data) -> checksum(Data, <<>>, 0).

checksum(<<H>>, Body, CheckSum) -> {H == CheckSum, Body, CheckSum};
checksum(<<H, T/binary>>, Body, CheckSum) -> checksum(T, <<Body/binary, H>>, CheckSum bxor H).

calc_checksum(Data) -> calc_checksum(Data, 0).
calc_checksum(<<>>, CheckSum) -> CheckSum;
calc_checksum(<<H, T/binary>>, CheckSum) -> calc_checksum(T, CheckSum bxor H).
