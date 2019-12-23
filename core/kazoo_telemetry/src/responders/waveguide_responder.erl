%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module (waveguide_responder).

-behaviour (gen_server).

-export([start_link/0
        ,ping/0, ping/1
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-define(HANDSHAKE_TIMER, 5000).

-include("waveguide.hrl").

-record(state, {activation_ts = 'undefined' :: non_neg_integer() | 'undefined'
               ,enabled = 'true' :: boolean()
               ,handshake_tref :: timer:tref()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ping() -> 'no_return'.
ping() -> ping(?WG_MAIN_PING).

-spec ping(kz_term:ne_binary()) -> 'no_return'.
ping(Ping) ->
    gen_server:call(?SERVER, {'ping', Ping}),

    %  of
    %     {'reply', {'ok', _Data} _} -> ok;
    %     {'reply', {'error', _}, _} -> ok
    % end,
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    TRef = erlang:start_timer(5000, self(), 'handshake'),
    {'ok', #state{activation_ts=activation_ts()
                 ,enabled=enabled()
                 ,handshake_tref=TRef
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'ping', ?WG_MAIN_PING=_Ping}, State) when State#state.enabled =:= 'true' ->
    % {'ok', Data} = waveguide_reducer:ping(Ping),
    lager:notice("Main Ping"),
    % {Op, _Msg} = wg_httpc:put(Data),
    {'noreply', State};
handle_cast({'ping', ?WG_HANDSHAKE_PING=_Ping}, State) ->
    lager:notice("Handshake Ping"),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', _Pid, 'main'}, State) ->
    main_ping(),
    {'noreply', State};
handle_info({'timeout', _Pid, 'handshake'}, State) ->
    NewState = case wg_util:days_remaining() of
        0 ->
            handshake_ping(),
            State;
        Days ->
            lager:debug("~p more days until waveguide activation", [Days]),
            TRef = erlang:start_timer(?DAY_IN_SECONDS, self(), 'handshake'),
            State#state{handshake_tref=TRef}
    end,
    {'noreply', NewState};
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec activation_ts() -> non_neg_integer().
activation_ts() ->
    maybe_activate(?WG_ACTIVATION).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_activate('undefined' | non_neg_integer()) -> non_neg_integer().
maybe_activate('undefined') ->
    Timestamp = kz_time:current_tstamp(),
    kapps_config:set_default(?WG_CAT, <<"activation">>, Timestamp),
    maybe_activate(Timestamp);
maybe_activate(Timestamp) when is_integer(Timestamp) -> Timestamp.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec enabled() -> boolean().
enabled() -> ?WG_ENABLED.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
handshake_ping() ->
    gen_server:cast(?SERVER, {'ping', ?WG_HANDSHAKE_PING}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
main_ping() ->
    gen_server:cast(?SERVER, {'ping', ?WG_MAIN_PING}).
