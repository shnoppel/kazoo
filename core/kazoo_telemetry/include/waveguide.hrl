    %%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-ifndef(WAVEGUIDE_HRL).

-include("kazoo_telemetry.hrl").

-define(DAY_IN_SECONDS, 86400).

-define(WG_CAT, <<(?TELEMETRY_CAT)/binary,".waveguide">>).

-define(WG_HANDSHAKE_PING, <<"handshake">>).
-define(WG_MAIN_PING, <<"main">>).
-define(WG_VERSION, <<"0.1.0">>).
-define(WG_USER_AGENT, <<"waveguide/",(?WG_VERSION)/binary>>).

-define(ANONYMIZE(Binary), kz_binary:hexencode(crypto:hash(?ANONYMIZE_HASH_ALG, Binary))).
-define(ANONYMIZE_HASH_ALG
       ,kapps_config:get_atom(?WG_CAT, <<"hash_algorithm">>, 'sha', <<"default">>)
       ).

-define(WG_ACTIVATION
       ,kapps_config:get_integer(?WG_CAT, <<"activation">>, 'undefined', <<"default">>)
       ).

-define(WG_ANONYMIZE_CLUSTER
       ,kapps_config:get_boolean(?WG_CAT, <<"cluster_id_anonymized">>, 'true', <<"default">>)
       ).

-define(WG_ENABLED
       ,kapps_config:get_boolean(?WG_CAT, <<"waveguide_enabled">>, 'true', <<"default">>)
       ).

-define(WG_GRACE_PERIOD
       ,kapps_config:get_integer(?WG_CAT, <<"grace_period">>, 1209600, <<"default">>)
       ).

-define(WG_INCLUDE_SERVICES
       ,kapps_config:get_boolean(?WG_CAT, <<"include_services">>, 'true', <<"default">>)
       ).

-define(WG_URL
       ,kapps_config:get_ne_binary(?WG_CAT, <<"waveguide_url">>, <<"http://waveguide.p.zswitch.net">>, <<"default">>)
       ).

-define(WAVEGUIDE_HRL, 'true').
-endif.
