src/Elixir.RabbitMQ.CLI.Ctl.Commands.ListMqttConnectionsCommand.erl:: include/rabbit_mqtt.hrl; @touch $@
src/jt808_frame.erl:: include/jt808_frame.hrl; @touch $@
src/rabbit_mqtt_frame.erl:: include/rabbit_mqtt_frame.hrl; @touch $@
src/rabbit_mqtt_processor.erl:: include/rabbit_mqtt.hrl include/rabbit_mqtt_frame.hrl; @touch $@
src/rabbit_mqtt_reader.erl:: include/rabbit_mqtt.hrl; @touch $@
src/rabbit_mqtt_retained_msg_store_dets.erl:: include/rabbit_mqtt.hrl src/rabbit_mqtt_retained_msg_store.erl; @touch $@
src/rabbit_mqtt_retained_msg_store_ets.erl:: include/rabbit_mqtt.hrl src/rabbit_mqtt_retained_msg_store.erl; @touch $@
src/rabbit_mqtt_retained_msg_store_noop.erl:: include/rabbit_mqtt.hrl src/rabbit_mqtt_retained_msg_store.erl; @touch $@
src/rabbit_mqtt_retainer.erl:: include/rabbit_mqtt.hrl include/rabbit_mqtt_frame.hrl; @touch $@
src/rabbit_mqtt_util.erl:: include/rabbit_mqtt.hrl; @touch $@

COMPILE_FIRST += rabbit_mqtt_retained_msg_store
