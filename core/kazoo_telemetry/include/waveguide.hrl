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

-define(WG_DEF_ANONYMIZE_CLUSTER, 'true').
-define(WG_DEF_SERVICES, 'true').

-define(DEF_INTERVAL_S, 86400).

-define(ANONYMIZE_HASH_ALG, 'md5').
-define(ANONYMIZE(Binary), crypto:hash(?ANONYMIZE_HASH_ALG, Binary)).

-define(WG_VERSION, <<"0.1.0">>).
-define(WG_USER_AGENT, <<"waveguide/",(?WG_VERSION)/binary>>).

-define(WAVEGUIDE_HRL, 'true').
-endif.
