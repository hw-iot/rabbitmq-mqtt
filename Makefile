PROJECT = rabbitmq_jt808
PROJECT_DESCRIPTION = RabbitMQ JT808 Adapter
PROJECT_MOD = huwo_jt808

define PROJECT_ENV
[
			{log, [{file, [{level, debug}]}]},
			{default_user, <<"guest">>},
			{default_pass, <<"guest">>},
			{ssl_cert_login,false},
			%% To satisfy an unfortunate expectation from popular JT808 clients.
			{allow_anonymous, true},
			{vhost, <<"/">>},
			{exchange, <<"amq.topic">>},
			{subscription_ttl, 86400000}, %% 24 hours
			{retained_message_store, huwo_jt808_retained_msg_store_dets},
			%% only used by DETS store
			{retained_message_store_dets_sync_interval, 2000},
			{prefetch, 10},
			{ssl_listeners, []},
			{num_ssl_acceptors, 1},
			{tcp_listeners, [8898]},
			{num_tcp_acceptors, 10},
			{tcp_listen_options, [{backlog,   128},
														{nodelay,   true}]},
			{proxy_protocol, false}
		]
endef

define PROJECT_APP_EXTRA_KEYS
	{broker_version_requirements, []}
endef

DEPS = ranch rabbit_common rabbit amqp_client ranch_proxy_protocol
TEST_DEPS = emqttc ct_helper rabbitmq_ct_helpers rabbitmq_ct_client_helpers

dep_ct_helper = git https://github.com/extend/ct_helper.git master
dep_emqttc = git https://github.com/emqtt/emqttc.git master

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

ELIXIR_LIB_DIR = $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))')
ifeq ($(ERL_LIBS),)
	ERL_LIBS = $(ELIXIR_LIB_DIR)
else
	ERL_LIBS := $(ERL_LIBS):$(ELIXIR_LIB_DIR)
endif
# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk

ifdef debug
	ERLC_OPTS := -Dbin_debug $(ERLC_OPTS)
endif

clean::
	if test -d test/java_SUITE_data; then cd test/java_SUITE_data && $(MAKE) clean; fi

tags::
	find . -type f -iname "*.erl" -o -iname "*.hrl" | etags -

docker:: clean dist
	mv ./plugins/rabbitmq_jt808-3.7.0*.ez ./plugins/rabbitmq_jt808-3.7.0.ez
	docker build ./ -t huwo/rabbitmq:latest
	docker tag huwo/rabbitmq:latest huwo/rabbitmq:3.7.4
	docker push huwo/rabbitmq:3.7.4
	docker push huwo/rabbitmq:latest

docker-local:: clean dist
	mv ./plugins/rabbitmq_jt808-3.7.0*.ez ./plugins/rabbitmq_jt808-3.7.0.ez
	docker build ./ -t hub.huwo.io/library/rabbitmq:latest
	docker tag hub.huwo.io/library/rabbitmq:latest hub.huwo.io/library/rabbitmq:3.7.4
	docker push hub.huwo.io/library/rabbitmq:3.7.4
	docker push hub.huwo.io/library/rabbitmq:latest
