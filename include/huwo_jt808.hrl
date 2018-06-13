%% -define(debug, true).

-ifdef(debug).
-define(DEBUG(Key, Val), bin_utils:dump(Key, Val)).
-else.
-define(DEBUG(Key, Val), true).
-endif.

-define(CLIENT_ID_MAXLEN, 23).

%% reader state
-record(state,      { socket,
                      conn_name,
                      await_recv,
                      deferred_recv,
                      received_connect_frame,
                      connection_state,
                      keepalive,
                      keepalive_sup,
                      conserve,
                      parse_state,
                      proc_state,
                      connection,
                      stats_timer }).

%% processor state
-record(proc_state, { socket,
                      subscriptions,
                      consumer_tags,
                      unacked_pubs,
                      awaiting_ack,
                      awaiting_seqno,
                      message_id,
                      client_id,
                      clean_sess,
                      will_msg,
                      channels,
                      connection,
                      exchange,
                      adapter_info,
                      ssl_login_name,
                      %% Retained messages handler. See huwo_jt808_retainer_sup
                      %% and huwo_jt808_retainer.
                      retainer_pid,
                      auth_state,
                      send_fun}).

-record(auth_state, {username,
                     user,
                     vhost}).

%% does not include vhost: it is used in
%% the table name
-record(retained_message, {topic,
                           mqtt_msg}).

-define(INFO_ITEMS,
    [host,
     port,
     peer_host,
     peer_port,
     protocol,
     channels,
     channel_max,
     frame_max,
     client_properties,
     ssl,
     ssl_protocol,
     ssl_key_exchange,
     ssl_cipher,
     ssl_hash,
     conn_name,
     connection_state,
     connection,
     consumer_tags,
     unacked_pubs,
     awaiting_ack,
     awaiting_seqno,
     message_id,
     client_id,
     clean_sess,
     will_msg,
     exchange,
     ssl_login_name,
     retainer_pid,
     user,
     vhost]).
