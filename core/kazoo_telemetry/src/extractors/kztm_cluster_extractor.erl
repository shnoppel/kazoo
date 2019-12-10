%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kztm_cluster_extractor).

-behaviour(gen_extractor).

-export([extract/0]).

%%------------------------------------------------------------------------------
%% @doc gen_extractor callback
%% @end
%%------------------------------------------------------------------------------
-spec extract() -> kz_term:proplists().
extract() ->
    ClusterJObj = cluster_id(),
    [{<<"cluster">>, ClusterJObj}].

%%------------------------------------------------------------------------------
%% @doc Generate cluster object body
%% @end
%%------------------------------------------------------------------------------
-spec cluster_id() -> kz_term:proplists().
cluster_id() ->
    [{<<"cluster_id">>, kzd_cluster:id()}].
