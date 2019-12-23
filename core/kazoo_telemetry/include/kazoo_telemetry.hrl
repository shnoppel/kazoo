%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-ifndef(KAZOO_TELEMETRY_HRL).

-define(TELEMETRY_CAT, <<"telemetry">>).

-define(TM_DEFAULT_LEADER_TICK, 60000).
-define(TM_DEFAULT_LEADER_APP, <<"ecallmgr">>).
-define(TM_DEFAULT_RESPONDERS, [<<"waveguide_responder">>]).

-define(TM_LEADER_TICK
       ,kapps_config:get_integer(?TELEMETRY_CAT, <<"leader_tick_ms">>, ?TM_DEFAULT_LEADER_TICK, <<"default">>)
       ).

-define(TM_LEADER_APP
       ,kapps_config:get_ne_binary(?TELEMETRY_CAT, <<"leader_app">>, ?TM_DEFAULT_LEADER_APP, <<"default">>)
       ).

-define(TM_RESPONDERS
       ,kapps_config:get_ne_binaries(?TELEMETRY_CAT, <<"responders">>, ?TM_DEFAULT_RESPONDERS, <<"default">>)
       ).

-define(KAZOO_TELEMETRY_HRL, 'true').
-endif.
