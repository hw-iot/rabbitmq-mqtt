FROM rabbitmq:3-management

ADD ./plugins/rabbitmq_jt808-3.7.0.ez /usr/lib/rabbitmq/lib/rabbitmq_server-3.7.6/plugins/
RUN rabbitmq-plugins enable --offline rabbitmq_jt808 rabbitmq_mqtt rabbitmq_web_mqtt
