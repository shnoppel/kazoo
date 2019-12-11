%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kztm_services_extractor).

-behaviour(gen_extractor).

-export ([extract/0]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec extract() -> kz_term:proplist().
extract() ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    Services = kz_services:fetch(MasterAccountId),
    [{<<"service_items">>, kz_services_quantities:fetch_cascade(Services)}].
