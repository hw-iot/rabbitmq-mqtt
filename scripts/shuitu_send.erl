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

    {RoutingKey, Message} = case Argv of
                                [] ->
                                    {<<"anonymous.info">>, <<"Hello World!">>};
                                [R] ->
                                    {list_to_binary(R), <<"Hello World!">>};
                                [R | Msg] ->
                                    {list_to_binary(R), list_to_binary(string:join(Msg, " "))}
                            end,
    amqp_channel:cast(Channel,
                      #'basic.publish'{
                        exchange = <<"amq.topic">>,
                        routing_key = RoutingKey},
                      #amqp_msg{payload = Message}),
    io:format(" [x] Sent ~p:~p~n", [RoutingKey, Message]),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.
