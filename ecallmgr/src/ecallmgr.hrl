-include("../include/amqp_client/include/amqp_client.hrl").
-include("whistle_types.hrl").

-record(handler_stats, {lookups_success = 0 :: integer()
			,lookups_failed = 0 :: integer()
                        ,lookups_timeout = 0 :: integer()
                        ,lookups_requested = 0 :: integer()
			,started = {0,0,0} :: tuple(integer(), integer(), integer())
		       }).

-record(node_stats, {started = {0,0,0} :: tuple(integer(), integer(), integer())
		     ,last_heartbeat = {0,0,0} :: tuple(integer(), integer(), integer())
                     ,created_channels = 0 :: integer()
		     ,destroyed_channels = 0 :: integer()
		     ,fs_uptime = 0 :: integer() % in microseconds
		    }).

-define(DEFAULT_DOMAIN, <<"trunks.2600hz.com">>).
-define(MAX_TIMEOUT_FOR_NODE_RESTART, 300000). % 5 minutes
