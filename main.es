#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -Wall
%%! -smp enable -sname factorial -mnesia debug verbose

-compile(export_all).

-include("include/huwo_jt808_frame.hrl").

test_huwo_jt808_frame()->
    PackageOrigin = #huwo_jt808_frame{
                       header = #huwo_jt808_frame_header{
                                   id = 42,
                                   timestamp = 201806011200,
                                   sn = 1},
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

-define(PARSE_STRING0(Payload, Key, Rest), [Key, Rest] = binary:split(Payload, [<<0,0>>])).
-define(PARSE_INT8(Payload, Key, Rest), << Key:8, Rest/binary >> = Payload).


test_serialise_connect_frame()->
    Frame = huwo_jt808_frame:serialise(
              #huwo_jt808_frame_connect{
		 mobile = "13896079527",
		 client_name = "huwo-erlang-jt808-client",
                 username = "user",
                 password = "pass",
                 client_type = 0,
                 phone_model = "iPhone 3G",
                 proto_ver = "2.1.1",
                 phone_os = "OSX 10",
                 work_mode = 1
                }),
    bin_utils:dump(frame, Frame),

    %% parse connect frame
    {ok, PackageParsed} = huwo_jt808_frame:parse(Frame),
    huwo_jt808_frame:dump(PackageParsed),
    Payload = PackageParsed#huwo_jt808_frame.payload,
    bin_utils:dump(payload, Payload),

    ?PARSE_STRING0(Payload,  Mobile,      Rest1),
    ?PARSE_STRING0(Rest1,    ClientName,  Rest2),
    ?PARSE_STRING0(Rest2,    Username,    Rest3),
    ?PARSE_STRING0(Rest3,    Password,    Rest4),
    ?PARSE_INT8   (Rest4,    ClientType,  Rest5),
    ?PARSE_STRING0(Rest5,    PhoneModel,  Rest6),
    ?PARSE_STRING0(Rest6,    ProtoVer,    Rest7),
    ?PARSE_INT8   (Rest7,    WorkMode,    Rest8),
    Record = #huwo_jt808_frame_connect{
		mobile = Mobile,
		client_name = ClientName,
		username = Username,
		password = Password,
		client_type = ClientType,
		phone_model = PhoneModel,
		proto_ver = ProtoVer,
		%% phone_os = PhoneOS,
		work_mode = WorkMode
	       },
    bin_utils:dump(record, Record),
    ok.

test_parse_connect_frame()->
    Sep = <<0,0>>,
    Mobile = <<"13896079527">>,
    Client_Name = <<"huwo-erlang-jt808-client">>,
    Data = << Mobile/binary, Sep/binary, Client_Name/binary, Sep/binary >>,

    bin_utils:dump(str, Data),

    [M1, Rest1] = binary:split(Data, [Sep]),
    [A1, Rest2] = binary:split(Rest1, [Sep]),
    bin_utils:dump(m, M1),
    bin_utils:dump(a, A1),
    bin_utils:dump(r, Rest2).

main(_) ->
    %% test_huwo_jt808_frame(),
    test_serialise_connect_frame(),
    io:fwrite("~n").
