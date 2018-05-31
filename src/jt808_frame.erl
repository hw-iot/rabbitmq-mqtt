%% TODO segment

-module(jt808_frame).

-author("Luo Tao <lotreal@gmail.com>").

-export([parse/1, serialise/1]).
-export([dump/1]).

-include("jt808_frame.hrl").

%% API
parse(Package) ->
    Content = parse_content(Package, ?FLAG_BOUNDARY),
    <<Id:16, Property:2/binary, Timestamp:6/binary, SN:16, Rest/binary>> = Content,
    <<Aes:1, Zip:1, Divide:1, Len:13>> = Property,
    <<Payload:Len/binary, _:8>> = Rest,
    {ok, #jt808_frame{
            header  = #jt808_frame_header{
                         id        = Id,
                         aes       = Aes,
                         zip       = Zip,
                         divide    = Divide,
                         length    = Len,
                         timestamp = bin_utils:bcd_decode(Timestamp),
                         sn        = SN},
            payload = Payload}}.

serialise(#jt808_frame{
             header = #jt808_frame_header{
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
dump(#jt808_frame{
              header = #jt808_frame_header{
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
