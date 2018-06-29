#!/usr/bin/env escript
%%! -pz /opt/rabbitmq/amqp_client /opt/rabbitmq/rabbit_common /opt/rabbitmq/amqp_client/ebin /opt/rabbitmq/rabbit_common/ebin /opt/rabbitmq/recon/ebin

-include_lib("amqp_client/include/amqp_client.hrl").

main(Argv) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{
                                 username = <<"gmsmq">>,
                                 password = <<"gmsmq">>,
                                 host = "172.29.0.20"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"amq.topic">>,
                                                   durable=true,
                                                   type = <<"topic">>}),

    #'queue.declare_ok'{queue = Queue} =
        amqp_channel:call(Channel, #'queue.declare'{exclusive = true,
                                                    durable=true}),

    [amqp_channel:call(Channel, #'queue.bind'{exchange = <<"amq.topic">>,
                                              routing_key = list_to_binary(BindingKey),
                                              queue = Queue})
     || BindingKey <- Argv],

    io:format(" [*] Waiting for logs. To exit press CTRL+C~n"),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{routing_key = RoutingKey}, #amqp_msg{payload = Body}} ->
            io:format(" [x] ~p:~p~n", [RoutingKey, Body]),
            %% bin_utils:dump(websocket, Body),
            loop(Channel)
    end.
