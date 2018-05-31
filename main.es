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

main(_) ->
    test_huwo_jt808_frame(),
    %% Package = <<16,35,0,6,77,81,73,115,100,112,3,2,0,60,0,21,109,111,115,113,112,117,98,
    %%             124,56,51,55,48,48,45,109,111,114,103,97,110,97>>,

    %% bin_utils:dump(package, Package),
    %% bin_utils:dump(response, jt808_send(Package)),

    io:fwrite("~n").

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
