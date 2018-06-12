FROM rabbitmq:3-management

ADD ./plugins/rabbitmq_jt808-3.7.0+71.g955f38a.ez /usr/lib/rabbitmq/lib/rabbitmq_server-3.7.5/plugins/
RUN rabbitmq-plugins enable --offline rabbitmq_jt808 # rabbitmq_mqtt
