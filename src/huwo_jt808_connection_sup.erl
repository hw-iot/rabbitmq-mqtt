%%  Copyright © 2018 LUO TAO <lotreal@gmail.com>
%%
%%   ___  ___  ___  ___  ___       __   ________  ___  ________  _________
%%  |\  \|\  \|\  \|\  \|\  \     |\  \|\   __  \|\  \|\   __  \|\___   ___\
%%  \ \  \\\  \ \  \\\  \ \  \    \ \  \ \  \|\  \ \  \ \  \|\  \|___ \  \_|
%%   \ \   __  \ \  \\\  \ \  \  __\ \  \ \  \\\  \ \  \ \  \\\  \   \ \  \
%%    \ \  \ \  \ \  \\\  \ \  \|\__\_\  \ \  \\\  \ \  \ \  \\\  \   \ \  \
%%     \ \__\ \__\ \_______\ \____________\ \_______\ \__\ \_______\   \ \__\
%%      \|__|\|__|\|_______|\|____________|\|_______|\|__|\|_______|    \|__|
%%
-module(huwo_jt808_connection_sup).

-behaviour(supervisor2).
-behaviour(ranch_protocol).

-include_lib("rabbit_common/include/rabbit.hrl").

-export([start_link/4, start_keepalive_link/0]).

-export([init/1]).

%%----------------------------------------------------------------------------
%% called by ranch_conns_sup:loop/4({acceptor, {0,0,0,0,0,0,0}, 1883}, #Port<rabbit@morganna.18500>, ranch_tcp, [])
start_link(Ref, Sock, _Transport, []) ->
    {ok, SupPid} = supervisor2:start_link(?MODULE, []),
    %% {ok, #Pid<rabbit@morgana.912.0>}
    {ok, KeepaliveSup} = supervisor2:start_child(
                          SupPid,
                          {huwo_jt808_keepalive_sup,
                           {huwo_jt808_connection_sup, start_keepalive_link, []},
                           intrinsic, infinity, supervisor, [rabbit_keepalive_sup]}),
    {ok, ReaderPid} = supervisor2:start_child(
                        SupPid,
                        {huwo_jt808_reader,
                         {huwo_jt808_reader, start_link, [KeepaliveSup, Ref, Sock]},
                         intrinsic, ?WORKER_WAIT, worker, [huwo_jt808_reader]}),
    %% {ok, #Pid<rabbit@morgana.911.0>, #Pid<rabbit@morgana.913.0>}
    {ok, SupPid, ReaderPid}.

start_keepalive_link() ->
    supervisor2:start_link(?MODULE, []).

%%----------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_all, 0, 1}, []}}.
