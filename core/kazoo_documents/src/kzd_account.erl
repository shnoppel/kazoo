%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_account).

-export([new/0
        ,type/0
        ,id/1
        ,fetch/1, fetch/2

        ,get_inherited_value/2
        ,get_inherited_value/3

        ,fetch_name/1, name/1, name/2, set_name/2
        ,fetch_realm/1, realm/1, realm/2, set_realm/2
        ,language/1, language/2, set_language/2
        ,timezone/1, timezone/2, set_timezone/2
        ,parent_account_id/1
        ,get_parent_account/1, get_parent_account_id/1
        ,tree/1, tree/2 ,set_tree/2
        ,notification_preference/1, set_notification_preference/2
        ,is_enabled/1, enable/1, disable/1
        ,api_key/1, set_api_key/2
        ,is_superduper_admin/1, set_superduper_admin/2
        ,allow_number_additions/1, set_allow_number_additions/2
        ,trial_expiration/1, trial_expiration/2, set_trial_expiration/2
        ,trial_time_left/1, trial_time_left/2
        ,trial_has_expired/1, trial_has_expired/2
        ,is_expired/1
        ,is_trial_account/1
        ,is_reseller/1, promote/1, demote/1
        ,reseller_id/1, set_reseller_id/2

        ,dial_plan/1, dial_plan/2
        ,fax_settings/1
        ,low_balance_threshold/1, low_balance_threshold/2, set_low_balance_threshold/2
        ,low_balance_sent/1, set_low_balance_sent/1, reset_low_balance_sent/1
        ,low_balance_enabled/1, set_low_balance_enabled/1, reset_low_balance_enabled/1, low_balance_enabled_exists/1
        ,low_balance_tstamp/1, set_low_balance_tstamp/1, set_low_balance_tstamp/2, remove_low_balance_tstamp/1
        ,topup_threshold/1, topup_threshold/2, set_topup_threshold/2
        ,sent_initial_registration/1, set_initial_registration_sent/2
        ,sent_initial_call/1, set_initial_call_sent/2
        ,home_zone/1, home_zone/2, set_home_zone/2

        ,preflow_id/1

         %% Account naming-related functionality
        ,format_account_id/1, format_account_id/2, format_account_id/3
        ,format_account_mod_id/1, format_account_mod_id/2, format_account_mod_id/3
        ,format_account_db/1
        ,format_account_modb/1, format_account_modb/2

        ,normalize_name/1
        ,account_update/1, account_update/2

        ,get_all_accounts/0, get_all_accounts/1
        ,get_all_accounts_and_mods/0, get_all_accounts_and_mods/1
        ,is_account_db/1, is_account_mod/1
        ,is_in_account_hierarchy/2, is_in_account_hierarchy/3
        ,is_account_enabled/1, is_account_expired/1

        ,maybe_disable_account/1
        ,disable_account/1
        ,enable_account/1
        ,is_system_admin/1

        ,update_superduper_admin/2
        ,update_allow_number_additions/2
        ]).

-include("kz_documents.hrl").

-define(ID, <<"_id">>).
-define(NAME, <<"name">>).
-define(REALM, <<"realm">>).
-define(LANGUAGE, <<"language">>).
-define(TIMEZONE, <<"timezone">>).
-define(THRESHOLD, <<"threshold">>).
-define(HOME_ZONE, [<<"zones">>, <<"home">>]).
-define(TREE, <<"pvt_tree">>).
-define(IS_ENABLED, <<"pvt_enabled">>).
-define(API_KEY, <<"pvt_api_key">>).
-define(IS_SUPERDUPER_ADMIN, <<"pvt_superduper_admin">>).
-define(ALLOW_NUMBER_ADDITIONS, <<"pvt_wnm_allow_additions">>).
-define(NOTIFY_CONFIG, <<"notifications">>).
-define(NOTIFY_VM_TO_EMAIL, [?NOTIFY_CONFIG, <<"voicemail_to_email">>]).
-define(NOTIFY_FAX_TO_EMAIL, [?NOTIFY_CONFIG, <<"fax_to_email">>]).
-define(NOTIFY_PREF, <<"pvt_notification_preference">>).
-define(KEY_TRIAL_EXPIRATION, <<"pvt_trial_expires">>).
-define(KEY_TRIAL_ACCOUNT, <<"is_trial_account">>).
-define(RESELLER, <<"pvt_reseller">>).
-define(RESELLER_ID, <<"pvt_reseller_id">>).
-define(KEY_DIAL_PLAN, <<"dial_plan">>).
-define(LOW_BALANCE_SENT, [<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>]).
-define(LOW_BALANCE_ENABLED, [<<"notifications">>, <<"low_balance">>, <<"enabled">>]).
-define(LOW_BALANCE_THRESHOLD, [<<"notifications">>, <<"low_balance">>, <<"threshold">>]).
-define(LOW_BALANCE_TSTAMP, [<<"notifications">>, <<"low_balance">>, <<"last_notification">>]).
-define(TOPUP_THRESHOLD, [<<"topup">>, <<"threshold">>]).
-define(SENT_INITIAL_REGISTRATION, [<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>]).
-define(SENT_INITIAL_CALL, [<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>]).

-define(REPLICATE_ENCODING, 'encoded').

-type doc() :: kz_json:object().

-export_type([doc/0
             ,account_format/0
             ]).

-spec get_inherited_value(api_binary(), fun()) -> any().
get_inherited_value(Account, ValueFun) ->
    get_inherited_value(Account, ValueFun, 'undefined').

-spec get_inherited_value(api_binary(), fun(), any()) -> any().
get_inherited_value('undefined', _ValueFun, Default) ->
    Default;

get_inherited_value(Account, ValueFun, Default) ->
    case check_account(Account, ValueFun) of
        'undefined' ->
            check_reseller(Account, ValueFun, Default);
        Value -> Value
    end.

-spec check_account(api_binary(), fun()) -> any().
check_account(Account, ValueFun) ->
    case fetch(Account) of
        {'error', _Err} -> 'undefined';
        {'ok', JObj} -> ValueFun(JObj)
    end.

-spec check_reseller(api_binary(), fun(), any()) -> any().
check_reseller(Account, ValueFun, Default) ->
    Reseller = kz_services:find_reseller_id(Account),
    case check_account(Reseller, ValueFun) of
        'undefined' -> Default;
        Value -> Value
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json:new(), type()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec type() -> ne_binary().
type() -> <<"account">>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec id(doc()) -> api_binary().
id(JObj) ->
    kz_doc:id(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(api_ne_binary()) -> {'ok', doc()} |
                                {'error', any()}.
fetch('undefined') ->
    {'error', 'invalid_db_name'};
fetch(Account=?NE_BINARY) ->
    fetch(Account, 'account').

-spec fetch(api_ne_binary(), 'account' | 'accounts') -> {'ok', doc()} |
                                                        {'error', any()}.
fetch('undefined', _) ->
    {'error', 'invalid_db_name'};
fetch(Account, 'account') ->
    AccountId = kzd_account:format_account_id(Account, 'raw'),
    AccountDb = kzd_account:format_account_id(Account, 'encoded'),
    open_cache_doc(AccountDb, AccountId);
fetch(AccountId, 'accounts') ->
    open_cache_doc(?KZ_ACCOUNTS_DB, AccountId).

-ifdef(TEST).
open_cache_doc(_, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    kz_json:fixture(?APP, <<"fixtures/account/", AccountId/binary, ".json">>).
-else.
open_cache_doc(Db, AccountId) ->
    kz_datamgr:open_cache_doc(Db, AccountId, [{cache_failures,false}]).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_name(api_ne_binary()) -> api_ne_binary().
fetch_name('undefined') -> 'undefined';
fetch_name(<<_/binary>>=Account) ->
    case fetch(Account) of
        {'ok', JObj} -> name(JObj);
        {'error', _R} ->
            lager:error("error opening account doc ~p", [Account]),
            'undefined'
    end.

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(JObj) ->
    name(JObj, 'undefined').
name(JObj, Default) ->
    kz_json:get_value(?NAME, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_name(doc(), ne_binary()) -> doc().
set_name(JObj, Name) ->
    kz_json:set_value(?NAME, Name, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_realm(ne_binary()) -> api_ne_binary().
fetch_realm(Account) ->
    case fetch(Account) of
        {ok, JObj} -> realm(JObj);
        {error, _R} ->
            lager:error("error opening account doc ~p", [Account]),
            undefined
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec realm(doc()) -> api_ne_binary().
-spec realm(doc(), Default) -> ne_binary() | Default.
realm(JObj) ->
    realm(JObj, 'undefined').
realm(JObj, Default) ->
    kz_json:get_ne_value(?REALM, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_realm(doc(), ne_binary()) -> doc().
set_realm(JObj, Realm) ->
    kz_json:set_value(?REALM, Realm, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec language(doc()) -> api_ne_binary().
-spec language(doc(), Default) -> ne_binary() | Default.
language(JObj) ->
    language(JObj, 'undefined').
language(JObj, Default) ->
    kz_json:get_ne_binary_value(?LANGUAGE, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_language(doc(), ne_binary()) -> doc().
set_language(JObj, Language) ->
    kz_json:set_value(?LANGUAGE, Language, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec timezone(api_ne_binary() | doc()) -> ne_binary().
timezone('undefined') -> kz_config_accounts:default_timezone();
timezone(AccountId) when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'ok', JObj} -> timezone(JObj);
        {'error', _R} ->
            lager:debug("failed to open account ~s definition, returning system's default timezone"),
            kz_config_accounts:default_timezone()
    end;
timezone(JObj) ->
    timezone(JObj, 'undefined').

-spec timezone(api_ne_binary() | doc(), Default) -> ne_binary() | Default.
timezone('undefined', 'undefined') ->
    kz_config_accounts:default_timezone();
timezone('undefined', <<"inherit">>) -> %% UI-1808
    kz_config_accounts:default_timezone();
timezone('undefined', Default) ->
    Default;
timezone(AccountId, Default) when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'ok', JObj} -> timezone(JObj, Default);
        {'error', _R} when Default =:= 'undefined';
                           Default =:= <<"inherit">> -> %% UI-1808
            lager:debug("failed to open account ~s definition, returning system's default timezone"),
            kz_config_accounts:default_timezone();
        {'error', _} ->
            Default
    end;
timezone(JObj, Default) ->
    case kz_json:get_value(?TIMEZONE, JObj, Default) of
        <<"inherit">> -> parent_timezone(kz_doc:account_id(JObj), parent_account_id(JObj)); %% UI-1808
        'undefined' -> parent_timezone(kz_doc:account_id(JObj), parent_account_id(JObj));
        TZ -> TZ
    end.

-spec parent_timezone(ne_binary(), api_ne_binary()) -> ne_binary().
parent_timezone(AccountId, AccountId) -> kz_config_accounts:default_timezone();
parent_timezone(_AccountId, 'undefined') -> kz_config_accounts:default_timezone();
parent_timezone(_AccountId, ParentId) -> timezone(ParentId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_timezone(doc(), ne_binary()) -> doc().
set_timezone(JObj, Timezone) ->
    kz_json:set_value(?TIMEZONE, Timezone, JObj).

-spec home_zone(ne_binary() | doc()) -> api_binary().
home_zone(AccountId) when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'error', _R} -> 'undefined';
        {'ok', JObj}  -> home_zone(JObj, 'undefined')
    end;

home_zone(JObj) ->
    home_zone(JObj, 'undefined').

-spec home_zone(ne_binary() | doc(), api_binary()) -> api_binary().
home_zone(AccountId, Default) when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'error', _R} -> Default;
        {'ok', JObj}  -> home_zone(JObj, Default)
    end;

home_zone(JObj, Default) ->
    kz_json:get_value(?HOME_ZONE, JObj, Default).

-spec set_home_zone(doc(), api_binary()) -> doc().
set_home_zone(JObj, Zone) ->
    kz_json:set_value(?HOME_ZONE, Zone, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_threshold(ne_binary() | doc()) -> api_float().
low_balance_threshold(Thing) ->
    Default = kapps_config:get_float(<<"notify.low_balance">>, <<"threshold">>, 5.00),
    low_balance_threshold(Thing, Default).

-spec low_balance_threshold(ne_binary() | doc(), Default) -> float() | Default.
low_balance_threshold(AccountId, Default) when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'error', _R} -> low_balance_threshold(kz_json:new(), Default);
        {'ok', JObj} -> low_balance_threshold(JObj, Default)
    end;
low_balance_threshold(JObj, Default) ->
    case kz_json:get_float_value(?LOW_BALANCE_THRESHOLD, JObj) of
        'undefined' -> topup_threshold(JObj, Default);
        Threshold -> Threshold
    end.

-spec set_low_balance_threshold(doc(), float()) -> doc().
set_low_balance_threshold(JObj, Threshold) ->
    kz_json:set_value(?LOW_BALANCE_THRESHOLD, Threshold, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_sent(doc()) -> boolean().
low_balance_sent(JObj) ->
    kz_json:is_true(?LOW_BALANCE_SENT, JObj).

-spec set_low_balance_sent(doc()) -> doc().
set_low_balance_sent(JObj) ->
    kz_json:set_value(?LOW_BALANCE_SENT, 'true', JObj).

-spec reset_low_balance_sent(doc()) -> doc().
reset_low_balance_sent(JObj) ->
    kz_json:set_value(?LOW_BALANCE_SENT, 'false', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_enabled(doc()) -> boolean().
low_balance_enabled(JObj) ->
    kz_json:is_true(?LOW_BALANCE_ENABLED, JObj).

-spec set_low_balance_enabled(doc()) -> doc().
set_low_balance_enabled(JObj) ->
    kz_json:set_value(?LOW_BALANCE_ENABLED, 'true', JObj).

-spec reset_low_balance_enabled(doc()) -> doc().
reset_low_balance_enabled(JObj) ->
    kz_json:set_value(?LOW_BALANCE_ENABLED, 'false', JObj).

-spec low_balance_enabled_exists(doc()) -> boolean().
low_balance_enabled_exists(JObj) ->
    kz_json:get_ne_value(?LOW_BALANCE_ENABLED, JObj) =/= 'undefined'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_tstamp(doc()) -> api_number().
low_balance_tstamp(JObj) ->
    kz_json:get_integer_value(?LOW_BALANCE_TSTAMP, JObj).

-spec set_low_balance_tstamp(doc()) -> doc().
set_low_balance_tstamp(JObj) ->
    TStamp = kz_time:now_s(),
    set_low_balance_tstamp(JObj, TStamp).

-spec set_low_balance_tstamp(doc(), number()) -> doc().
set_low_balance_tstamp(JObj, TStamp) ->
    kz_json:set_value(?LOW_BALANCE_TSTAMP, TStamp, JObj).

-spec remove_low_balance_tstamp(doc()) -> doc().
remove_low_balance_tstamp(JObj) ->
    kz_json:delete_key(?LOW_BALANCE_TSTAMP, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec topup_threshold(doc()) -> api_float().
topup_threshold(JObj) ->
    topup_threshold(JObj, 'undefined').

-spec topup_threshold(doc(), Default) -> float() | Default.
topup_threshold(JObj, Default) ->
    kz_json:get_float_value(?TOPUP_THRESHOLD, JObj, Default).

-spec set_topup_threshold(doc(), float()) -> doc().
set_topup_threshold(JObj, Threshold) ->
    kz_json:set_value(?TOPUP_THRESHOLD, Threshold, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec sent_initial_registration(doc()) -> boolean().
sent_initial_registration(JObj) ->
    kz_json:is_true(?SENT_INITIAL_REGISTRATION, JObj).

-spec set_initial_registration_sent(doc(), boolean()) -> doc().
set_initial_registration_sent(JObj, Sent) ->
    kz_json:set_value(?SENT_INITIAL_REGISTRATION, Sent, JObj).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec sent_initial_call(doc()) -> boolean().
sent_initial_call(JObj) ->
    kz_json:is_true(?SENT_INITIAL_CALL, JObj).

-spec set_initial_call_sent(doc(), boolean()) -> doc().
set_initial_call_sent(JObj, Sent) ->
    kz_json:set_value(?SENT_INITIAL_CALL, Sent, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parent_account_id(doc()) -> api_binary().
parent_account_id(JObj) ->
    case tree(JObj) of
        [] -> 'undefined';
        Ancestors -> lists:last(Ancestors)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_parent_account(ne_binary()) -> {'ok', doc()} | {'error', any()}.
get_parent_account(AccountId) ->
    case get_parent_account_id(AccountId) of
        'undefined' -> {'error', 'not_found'};
        ParentId ->
            fetch(ParentId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_parent_account_id(ne_binary()) -> api_binary().
get_parent_account_id(AccountId) ->
    case fetch(AccountId) of
        {'ok', JObj} -> parent_account_id(JObj);
        {'error', _R} ->
            lager:debug("failed to open account's ~s parent: ~p", [AccountId, _R]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tree(doc()) -> ne_binaries().
-spec tree(doc(), Default) -> ne_binaries() | Default.
tree(JObj) ->
    tree(JObj, []).
tree(JObj, Default) ->
    kz_json:get_list_value(?TREE, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_tree(doc(), ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    kz_json:set_value(?TREE, Tree, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec notification_preference(doc()) -> api_binary().
notification_preference(JObj) ->
    Pref = notification_preference(JObj, [
                                          ?NOTIFY_PREF
                                         ,?NOTIFY_VM_TO_EMAIL
                                         ,?NOTIFY_FAX_TO_EMAIL
                                         ]),

    case Pref of
        'undefined'    -> 'undefined';
        <<"teletype">> -> <<"teletype">>;
        _Default       -> <<"notify">>
    end.

-spec notification_preference(doc(), list()) -> api_binary().
notification_preference(_JObj, []) ->
    'undefined';

notification_preference(JObj, [H|T]) ->
    case kz_json:get_ne_value(H, JObj) of
        'undefined' ->
            notification_preference(JObj, T);
        Value ->
            Value
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_notification_preference(doc(), ne_binary()) -> doc().
set_notification_preference(JObj, Pref) ->
    kz_json:set_value(?NOTIFY_PREF, Pref, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(doc()) -> boolean().
is_enabled(JObj) ->
    kz_json:is_true(?IS_ENABLED, JObj, 'true').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec enable(doc()) -> doc().
enable(JObj) ->
    kz_json:set_value(?IS_ENABLED, 'true', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec disable(doc()) -> doc().
disable(JObj) ->
    kz_json:set_value(?IS_ENABLED, 'false', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec api_key(doc()) -> api_binary().
api_key(JObj) ->
    kz_json:get_value(?API_KEY, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_api_key(doc(), ne_binary()) -> doc().
set_api_key(JObj, ApiKey) ->
    kz_json:set_value(?API_KEY, ApiKey, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_superduper_admin(doc()) -> boolean().
is_superduper_admin(JObj) ->
    kz_json:is_true(?IS_SUPERDUPER_ADMIN, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_superduper_admin(doc(), boolean()) -> doc().
set_superduper_admin(JObj, IsAdmin) ->
    kz_json:set_value(?IS_SUPERDUPER_ADMIN, IsAdmin, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec allow_number_additions(doc()) -> boolean().
allow_number_additions(JObj) ->
    kz_json:is_true(?ALLOW_NUMBER_ADDITIONS, JObj).

-spec set_allow_number_additions(doc(), boolean()) -> doc().
set_allow_number_additions(JObj, IsAllowed) ->
    kz_json:set_value(?ALLOW_NUMBER_ADDITIONS, kz_term:is_true(IsAllowed), JObj).

-spec trial_expiration(doc()) -> api_integer().
-spec trial_expiration(doc(), Default) -> integer() | Default.
trial_expiration(JObj) ->
    trial_expiration(JObj, 'undefined').

trial_expiration(JObj, Default) ->
    kz_json:get_integer_value(?KEY_TRIAL_EXPIRATION, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_trial_expiration(doc(), gregorian_seconds()) -> doc().
set_trial_expiration(JObj, Expiration) ->
    JObj1 = kz_json:delete_key(?KEY_TRIAL_ACCOUNT, JObj),
    kz_json:set_value(?KEY_TRIAL_EXPIRATION, Expiration, JObj1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec trial_time_left(doc()) -> integer().
-spec trial_time_left(doc(), gregorian_seconds()) -> integer().
trial_time_left(JObj) ->
    trial_time_left(JObj, kz_time:now_s()).
trial_time_left(JObj, Now) ->
    case trial_expiration(JObj) of
        'undefined' -> 0;
        Expiration -> kz_time:elapsed_s(Now, Expiration)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec trial_has_expired(doc()) -> boolean().
-spec trial_has_expired(doc(), gregorian_seconds()) -> boolean().
trial_has_expired(JObj) ->
    trial_has_expired(JObj, kz_time:now_s()).
trial_has_expired(JObj, Now) ->
    trial_expiration(JObj) =/= 'undefined'
        andalso trial_time_left(JObj, Now) =< 0.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_expired(doc()) -> 'false' | {'true', gregorian_seconds()}.
is_expired(JObj) ->
    case trial_has_expired(JObj) of
        'false' -> 'false';
        'true' -> {'true', trial_expiration(JObj)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_trial_account(doc()) -> boolean().
is_trial_account(JObj) ->
    kz_json:is_true(?KEY_TRIAL_ACCOUNT, JObj, 'false').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_reseller(doc()) -> boolean().
is_reseller(JObj) ->
    kz_json:is_true(?RESELLER, JObj)
        orelse is_superduper_admin(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec promote(doc()) -> doc().
promote(JObj) ->
    kz_json:set_value(?RESELLER, 'true', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec demote(doc()) -> doc().
demote(JObj) ->
    kz_json:set_value(?RESELLER, 'false', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reseller_id(doc()) -> doc().
reseller_id(JObj) ->
    kz_json:get_value(?RESELLER_ID, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_reseller_id(doc(), ne_binary()) -> doc().
set_reseller_id(JObj, ResellerId) ->
    kz_json:set_value(?RESELLER_ID, ResellerId, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dial_plan(doc()) -> api_object().
-spec dial_plan(doc(), Default) -> kz_json:object() | Default.
dial_plan(JObj) ->
    dial_plan(JObj, 'undefined').
dial_plan(JObj, Default) ->
    kz_json:get_json_value(?KEY_DIAL_PLAN, JObj, Default).

-spec fax_settings(doc() | ne_binary()) -> doc().
fax_settings(AccountId)
  when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'ok', JObj} -> fax_settings(JObj);
        {'error', _} -> ?SYSTEM_FAX_SETTINGS
    end;
fax_settings(JObj) ->
    FaxSettings = kz_json:get_json_value(?FAX_SETTINGS_KEY, JObj, ?DEFAULT_FAX_SETTINGS),
    case kz_json:get_value(?FAX_TIMEZONE_KEY, FaxSettings) of
        'undefined' -> kz_json:set_value(?FAX_TIMEZONE_KEY, timezone(JObj), FaxSettings);
        _ -> FaxSettings
    end.

-spec preflow_id(doc()) -> api_ne_binary().
-spec preflow_id(doc(), Default) -> ne_binary() | Default.
preflow_id(Doc) ->
    preflow_id(Doc, 'undefined').

preflow_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"preflow">>, <<"always">>], Doc, Default).



%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an account return it in a 'encoded',
%% unencoded or 'raw' format.
%% Note: accepts MODbs as well as account IDs/DBs
%% Note: if given (Account, GregorianSeconds), it will return
%%   an MODb in the 'encoded' format.
%% @end
%%--------------------------------------------------------------------
-type account_format() :: 'unencoded' | 'encoded' | 'raw'.
-spec format_account_id(api_binary()) -> api_binary().
-spec format_account_id(api_binary(), account_format()) -> api_binary();
                       (api_binary(), gregorian_seconds()) -> api_binary(). %% MODb!

format_account_id(Account) ->
    format_account_id(Account, 'raw').

format_account_id('undefined', _Encoding) -> 'undefined';
format_account_id(DbName, Timestamp)
  when is_integer(Timestamp)
       andalso Timestamp > 0 ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    format_account_id(DbName, Year, Month);
format_account_id(<<"accounts">>, _) -> <<"accounts">>;

format_account_id(?MATCH_ACCOUNT_RAW(_)=AccountId, 'raw') ->
    AccountId;
format_account_id(?MATCH_ACCOUNT_ENCODED(_)=AccountDb, 'encoded') ->
    AccountDb;
format_account_id(?MATCH_ACCOUNT_UNENCODED(_)=AccountDbUn, 'unencoded') ->
    AccountDbUn;

format_account_id(AccountId, 'raw') ->
    raw_account_id(AccountId);
format_account_id(AccountId, 'unencoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_id(AccountId),
    kz_term:to_binary(["account/", A, "/", B, "/", Rest]);
format_account_id(AccountId, 'encoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_id(AccountId),
    kz_term:to_binary(["account%2F", A, "%2F", B, "%2F", Rest]).

%% @private
%% Returns account_id() | any()
%% Passes input along if not account_id() | account_db() | account_db_unencoded().
-spec raw_account_id(ne_binary()) -> ne_binary().
raw_account_id(?MATCH_ACCOUNT_RAW(AccountId)) ->
    AccountId;
raw_account_id(?MATCH_ACCOUNT_UNENCODED(A, B, Rest)) ->
    ?MATCH_ACCOUNT_RAW(A, B, Rest);
raw_account_id(?MATCH_ACCOUNT_ENCODED(A, B, Rest)) ->
    ?MATCH_ACCOUNT_RAW(A, B, Rest);
raw_account_id(?MATCH_MODB_SUFFIX_RAW(AccountId, _, _)) ->
    AccountId;
raw_account_id(?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, _, _)) ->
    ?MATCH_ACCOUNT_RAW(A, B, Rest);
raw_account_id(?MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, _, _)) ->
    ?MATCH_ACCOUNT_RAW(A, B, Rest);
raw_account_id(?MATCH_RESOURCE_SELECTORS_RAW(AccountId)) ->
    AccountId;
raw_account_id(?MATCH_RESOURCE_SELECTORS_UNENCODED(A, B, Rest)) ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest);
raw_account_id(?MATCH_RESOURCE_SELECTORS_ENCODED(A, B, Rest)) ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest);
raw_account_id(<<"number/", _/binary>>=Other) ->
    Other;
raw_account_id(Other) ->
    case lists:member(Other, ?KZ_SYSTEM_DBS) of
        'true' -> Other;
        'false' ->
            lager:warning("raw account id doesn't process '~p'", [Other]),
            Other
    end.

%% @private
%% (modb()) -> modb_id() when modb() :: modb_id() | modb_db() | modb_db_unencoded()
%% Crashes if given anything else.
-spec raw_account_modb(ne_binary()) -> ne_binary().
raw_account_modb(?MATCH_MODB_SUFFIX_RAW(_, _, _) = AccountId) ->
    AccountId;
raw_account_modb(?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, Year, Month)) ->
    ?MATCH_MODB_SUFFIX_RAW(A, B, Rest, Year, Month);
raw_account_modb(?MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, Year, Month)) ->
    ?MATCH_MODB_SUFFIX_RAW(A, B, Rest, Year, Month).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a list of all account database names
%% in the requested encoding
%% @end
%%--------------------------------------------------------------------
-spec get_all_accounts() -> ne_binaries().
-spec get_all_accounts(?MODULE:account_format()) -> ne_binaries().
get_all_accounts() -> get_all_accounts(?REPLICATE_ENCODING).

get_all_accounts(Encoding) ->
    {'ok', Dbs} = kz_datamgr:db_list_by_classifier('account'
                                                  ,[{'startkey', <<"account/">>}
                                                   ,{'endkey', <<"account0">>}
                                                   ]),
    [format_account_id(Db, Encoding)
     || Db <- Dbs
    ].

-spec get_all_accounts_and_mods() -> ne_binaries().
-spec get_all_accounts_and_mods(?MODULE:account_format()) -> ne_binaries().
get_all_accounts_and_mods() ->
    get_all_accounts_and_mods(?REPLICATE_ENCODING).

get_all_accounts_and_mods(Encoding) ->
    {'ok', Dbs} = kz_datamgr:db_list_by_classifier(['account', 'modb']
                                                  ,[{'startkey', <<"account/">>}
                                                   ,{'endkey', <<"account0">>}
                                                   ]),
    [format_db(Db, Encoding) || Db <- Dbs].

-spec format_db(ne_binary(), ?MODULE:account_format()) -> ne_binary().
format_db(Db, Encoding) ->
    Fs = [{fun is_account_db/1, fun ?MODULE:format_account_id/2}
         ,{fun is_account_mod/1, fun ?MODULE:format_account_modb/2}
         ],
    format_db(Db, Encoding, Fs).

format_db(Db, Encoding, [{Predicate, Formatter}|Fs]) ->
    case Predicate(Db) of
        'true' -> Formatter(Db, Encoding);
        'false' -> format_db(Db, Encoding, Fs)
    end.

-spec is_account_mod(ne_binary()) -> boolean().
is_account_mod(Db) ->
    kz_datamgr:db_classification(Db) =:= 'modb'.

-spec is_account_db(ne_binary()) -> boolean().
is_account_db(Db) ->
    kz_datamgr:db_classification(Db) =:= 'account'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an account, build an MODb in an 'encoded' format.
%% Note: accepts MODbs as well as account IDs/DBs
%% @end
%%--------------------------------------------------------------------
-spec format_account_id(api_binary(), kz_year() | ne_binary(), kz_month() | ne_binary()) ->
                               api_binary().
format_account_id('undefined', _Year, _Month) -> 'undefined';
format_account_id(AccountId, Year, Month) when not is_integer(Year) ->
    format_account_id(AccountId, kz_term:to_integer(Year), Month);
format_account_id(AccountId, Year, Month) when not is_integer(Month) ->
    format_account_id(AccountId, Year, kz_term:to_integer(Month));
format_account_id(Account, Year, Month) when is_integer(Year),
                                             is_integer(Month) ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_id(Account),
    ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, kz_term:to_binary(Year), kz_time:pad_month(Month)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an account, build an MODb in an 'encoded' format.
%% Note: accepts MODbs as well as account IDs/DBs
%% @end
%%--------------------------------------------------------------------
-spec format_account_mod_id(api_binary()) -> api_binary().
-spec format_account_mod_id(api_binary(), gregorian_seconds() | kz_now()) -> api_binary().
-spec format_account_mod_id(api_binary(), kz_year() | ne_binary(), kz_month() | ne_binary()) ->
                                   api_binary().

format_account_mod_id(Account) ->
    format_account_mod_id(Account, os:timestamp()).

format_account_mod_id(AccountId, {_,_,_}=Timestamp) ->
    {{Year, Month, _}, _} = calendar:now_to_universal_time(Timestamp),
    format_account_id(AccountId, Year, Month);
format_account_mod_id(AccountId, Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    format_account_id(AccountId, Year, Month).

format_account_mod_id(AccountId, Year, Month) ->
    format_account_id(AccountId, Year, Month).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an account return it in a 'encoded' format.
%% Note: accepts MODbs as well as account IDs/DBs
%% @end
%%--------------------------------------------------------------------
-spec format_account_db(api_binary()) -> api_binary().
format_account_db(AccountId) ->
    format_account_id(AccountId, 'encoded').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an MODb return the MODb in the specified format.
%% Note: crashes if given anything but an MODb (in any format).
%% @end
%%--------------------------------------------------------------------
-spec format_account_modb(ne_binary()) -> ne_binary().
-spec format_account_modb(ne_binary(), account_format()) -> ne_binary().
format_account_modb(AccountId) ->
    format_account_modb(AccountId, 'raw').
format_account_modb(AccountId, 'raw') ->
    raw_account_modb(AccountId);
format_account_modb(AccountId, 'unencoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_modb(AccountId),
    kz_term:to_binary(["account/", A, "/", B, "/", Rest]);
format_account_modb(AccountId, 'encoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_modb(AccountId),
    kz_term:to_binary(["account%2F", A, "%2F", B, "%2F", Rest]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Normalize the account name by converting the name to lower case
%% and then removing all non-alphanumeric characters.
%%
%% This can possibly return an empty binary.
%% @end
%%--------------------------------------------------------------------
-spec normalize_name(api_binary()) -> api_binary().
normalize_name('undefined') -> 'undefined';
normalize_name(AccountName) ->
    << <<Char>>
       || <<Char>> <= kz_term:to_lower_binary(AccountName),
          is_alphanumeric(Char)
    >>.

is_alphanumeric(Char)
  when Char >= $a,
       Char =< $z ->
    true;
is_alphanumeric(Char)
  when Char >= $0,
       Char =< $9 ->
    true;
is_alphanumeric(_) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Determine if the given account id/db exists in the hierarchy of
%% the provided account id/db. Optionally consider the account in
%% its own hierarchy.
%% @end
%%--------------------------------------------------------------------
-spec is_in_account_hierarchy(api_binary(), api_binary()) -> boolean().
-spec is_in_account_hierarchy(api_binary(), api_binary(), boolean()) -> boolean().

is_in_account_hierarchy(CheckFor, InAccount) ->
    is_in_account_hierarchy(CheckFor, InAccount, 'false').

is_in_account_hierarchy('undefined', _, _) -> 'false';
is_in_account_hierarchy(_, 'undefined', _) -> 'false';
is_in_account_hierarchy(CheckFor, InAccount, IncludeSelf) ->
    CheckId = format_account_id(CheckFor),
    AccountId = format_account_id(InAccount),
    case (IncludeSelf
          andalso AccountId =:= CheckId
         )
        orelse ?MODULE:fetch(AccountId)
    of
        'true' ->
            lager:debug("account ~s is the same as the account to fetch the hierarchy from", [CheckId]),
            'true';
        {'ok', JObj} ->
            Tree = ?MODULE:tree(JObj),
            case lists:member(CheckId, Tree) of
                'true' ->
                    lager:debug("account ~s is in the account hierarchy of ~s", [CheckId, AccountId]),
                    'true';
                'false' ->
                    lager:debug("account ~s was not found in the account hierarchy of ~s", [CheckId, AccountId]),
                    'false'
            end;
        {'error', _R} ->
            lager:debug("failed to get the ancestry of the account ~s: ~p", [AccountId, _R]),
            'false'
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% checks the pvt_enabled flag and returns 'false' only if the flag is
%% specificly set to 'false'.  If it is missing or set to anything else
%% return 'true'.  However, if we cant find the account doc then return
%% 'false'.
%% @end
%%--------------------------------------------------------------------
-spec is_account_enabled(api_binary()) -> boolean().
is_account_enabled('undefined') -> 'false';
is_account_enabled(Account) ->
    case ?MODULE:fetch(Account) of
        {'error', _E} ->
            lager:error("could not open account ~s", [Account]),
            'false';
        {'ok', JObj} ->
            ?MODULE:is_enabled(JObj)
    end.

-spec is_account_expired(api_binary()) -> 'false' | {'true', gregorian_seconds()}.
is_account_expired('undefined') -> 'false';
is_account_expired(Account) ->
    case ?MODULE:fetch(Account) of
        {'error', _R} ->
            lager:debug("failed to check if expired token auth, ~p", [_R]),
            'false';
        {'ok', JObj} ->
            ?MODULE:is_expired(JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_disable_account(ne_binary()) ->
                                   {'ok', kz_json:object()} |
                                   {'error', any()}.
maybe_disable_account(Account) ->
    case is_account_enabled(Account) of
        'false' -> 'ok';
        'true' ->
            disable_account(Account)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec disable_account(ne_binary()) ->
                             {'ok', kz_json:object()} |
                             {'error', any()}.
disable_account(Account) ->
    account_update(Account, fun ?MODULE:disable/1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec enable_account(ne_binary()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
enable_account(Account) ->
    account_update(Account, fun ?MODULE:enable/1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_system_admin(api_binary()) -> boolean().
is_system_admin('undefined') -> 'false';
is_system_admin(Account) ->
    case ?MODULE:fetch(Account) of
        {'ok', JObj} -> ?MODULE:is_superduper_admin(JObj);
        {'error', _R} ->
            lager:debug("unable to open account definition for ~s: ~p", [Account, _R]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_superduper_admin(ne_binary(), boolean()) ->
                                     {'ok', kz_json:object()} |
                                     {'error', any()}.
update_superduper_admin(Account, IsAdmin) ->
    account_update(Account, fun(J) -> ?MODULE:set_superduper_admin(J, IsAdmin) end).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_allow_number_additions(ne_binary(), boolean()) ->
                                           {'ok', kz_json:object()} |
                                           {'error', any()}.
update_allow_number_additions(Account, IsAllowed) ->
    account_update(Account, fun(J) -> set_allow_number_additions(J, IsAllowed) end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_update(doc()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
-spec account_update(ne_binary(), function()) -> 'ok' | {'error', any()}.
account_update(AccountJObj) ->
    AccountDb = kz_doc:account_db(AccountJObj),
    case kz_datamgr:ensure_saved(AccountDb, AccountJObj) of
        {'error', _R}=E -> E;
        {'ok', SavedJObj} ->
            kz_datamgr:ensure_saved(?KZ_ACCOUNTS_DB, SavedJObj)
    end.

account_update(Account, UpdateFun) ->
    case ?MODULE:fetch(Account) of
        {'error', _R}=E -> E;
        {'ok', AccountJObj} ->
            account_update(UpdateFun(AccountJObj))
    end.