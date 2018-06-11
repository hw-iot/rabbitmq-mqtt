#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -Wall
%%! -smp enable -sname factorial -mnesia debug verbose

-compile(export_all).

-include("include/huwo_jt808.hrl").
-include("include/huwo_jt808_frame.hrl").

%% TODO move to make tests
test_huwo_jt808_frame()->
    PackageOrigin = #huwo_jt808_frame{
                       header = #huwo_jt808_frame_header{
                                   message_id = 42,
                                   message_sn = 1},
                       payload = <<"hello, 808!", 16#3D, 16#3E>>},

    huwo_jt808_frame:dump(PackageOrigin),

    Frame = huwo_jt808_frame:serialise(PackageOrigin),
    bin_utils:dump(frame, Frame),

    {ok, PackageParsed} = huwo_jt808_frame:parse(Frame),
    huwo_jt808_frame:dump(PackageParsed).

%% jt808_client
jt808_send(Package) ->
    {ok, Socket} = gen_tcp:connect("localhost", 1883, [{active, false}, binary]),
    bin_utils:dump(socket, Socket),
    ok = gen_tcp:send(Socket, Package),
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    receive
        {tcp, Socket, Bin}->
            receive_data(Socket, [Bin|SoFar]);
        {tcp_closed, Socket} ->
            %% list_to_binary(reverse(SoFar))
            list_to_binary(SoFar)
    end.

test_send_package() ->
    %% Package = <<16,35,0,6,77,81,73,115,100,112,3,2,0,60,0,21,109,111,115,113,112,117,98,
    %%             124,56,51,55,48,48,45,109,111,114,103,97,110,97>>,

    %% bin_utils:dump(package, Package),
    %% bin_utils:dump(response, jt808_send(Package)),
    ok.

%% -define(PARSE_STRING0(Payload, Key, Rest), [Key, Rest] = binary:split(Payload, [<<0,0>>])).
%% -define(PARSE_UINT8(Payload, Key, Rest), << Key:8, Rest/binary >> = Payload).

gen_connect_frame() ->
    huwo_jt808_session:warp(
      {?SIGNIN, 1,
       #huwo_jt808_frame_signin{
          token = "1234567890A"}}).

test_serialise_connect_frame()->
    Request = gen_connect_frame(),
    Frame = huwo_jt808_frame:serialise(Request),
    bin_utils:dump(frame, Frame),

    Frame0 = <<126,1,2,0,11,1,50,0,0,0,3,0,12,49,50,51,52,53,54,55,56,57,48,65,116,126>>,
    ?DEBUG(parse, Frame0),
    ok.

test_parse_connect_frame()->
    Request0 = gen_connect_frame(),

    Frame = huwo_jt808_frame:serialise(Request0),
    bin_utils:dump(frame, Frame),

    %% parse connect frame
    {ok, Request, _Rest} = huwo_jt808_frame:parse(Frame, none),
    huwo_jt808_frame:dump(Request).

test_gen_ack()->
    Connect = gen_connect_frame(),
    Ack = huwo_jt808_session:response(Connect, ?CONNACK_ACCEPT),
    bin_utils:dump(connect, {Connect, Ack}),

    Frame = huwo_jt808_frame:serialise(Ack),
    bin_utils:dump(frame, Frame).

test_frame_unknown()->
    Request0 = huwo_jt808_session:warp(
                 {42, 1,
                  #huwo_jt808_frame_unknown{
                     foo = "xinyi",
                     bar = "lee"}}),
    Frame = huwo_jt808_frame:serialise(Request0),
    bin_utils:dump(frame, Frame),

    {ok, Request, _Rest} = huwo_jt808_frame:parse(Frame, none),
    huwo_jt808_frame:dump(Request).

test_data_type() ->
    Uint8  = 254,
    Uint16 = 254,
    Uint32 = 254,

    Bin = <<?UINT8_OF(Uint8), ?UINT16_OF(Uint16), ?UINT32_OF(Uint32),
            ?UINT_OF(Uint8, 8)>>,
    ?DEBUG(dt, Bin).

serialise_header() ->
    MessageID = 16#0100,
    Mobile = 13896079527,
    MessageSN = 42,

    Segmentation = ?NO_SEGMENT,
    Encryption = ?NO_ENCRYPT,
    Length = 1,

    <<?UINT16_OF(MessageID),
      ?WORD_OF(<<Segmentation:3, Encryption:3, Length:10>>),
      ?BCD_OF(Mobile, 6),
      ?UINT16_OF(MessageSN)>>.

test_serialise_v2() ->
    Header = serialise_header(),
    <<?UINT16(MessageID),
      ?WORD(MessageProperty),
      ?BCD(Mobile0, 6),
      ?UINT16(MessageSN),
      ?BYTE(_)>> = Header,
    <<_Reserved:2, Segmentation:1, Encryption:3, Length:10>> = MessageProperty,
    Mobile = ?BCD_VALUE(Mobile0),
    ?DEBUG(parse, {MessageID, MessageSN, Mobile, Segmentation, Encryption, Length}).


test_checksum() ->
    Frame = <<1,2,0,11,1,50,0,0,0,3,0,12,49,50,51,52,53,54,55,56,57,48,65,116>>,
    ?DEBUG(checksum, huwo_jt808_frame:checksum(Frame)).

test_reader() ->
    Frame = <<126,1,2,0,11,1,50,0,0,0,3,0,12,49,50,51,52,53,54,55,56,57,48,65,116,126>>,
    ?DEBUG(parse, Frame),
    {ok, PackageParsed, _Rest} = huwo_jt808_frame:parse(Frame, none),
    ?DEBUG(parse,{ok, PackageParsed}),

    %% huwo_jt808_reader:process_received_bytes(Bin)
    ok.

main(_) ->
    %% test_huwo_jt808_frame(),
    test_serialise_connect_frame(),
    %% test_parse_connect_frame(),
    %% test_gen_ack(),
    %% test_frame_unknown(),
    %% test_data_type(),
    %% test_serialise_v2(),
    %% test_reader(),
    %% test_checksum(),
    io:fwrite("~n").
